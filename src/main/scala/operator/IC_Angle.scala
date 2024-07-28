package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import wa.ip._
import spinal.lib.experimental.chisel.Module
import spinal.core.sim._

import scala.collection.mutable.ListBuffer
import scala.math.{abs, ceil, floor, sqrt}

case class IC_AngleConfig(DATA_NUM : Int = 31,
                              MEM_DEPTH : Int = 1024,
                              SIZE_WIDTH : Int = 11
                             ) {
    val WINDOWS_SIZE_H = 7
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    //创建的MEM个数
    val umax = Array(15, 15, 15, 15, 14, 14, 14, 13, 13, 12, 11, 10, 9, 8, 6, 3)
    val HALF_PATCH_SIZE: Int = 15

}
//求得角度
class IC_Angle(config:IC_AngleConfig) extends Module{
    val io = new Bundle {//要求valid有效之后数据需要保持8周期的有效值//
        val sData = slave Flow (Vec(Vec(UInt(config.DATA_WIDTH bits), config.DATA_NUM), config.DATA_NUM)) //第一个是得分点,其余的是周围的点
        val mData = master Flow Vec(SInt(20 bits), 2)//传出M01和M10两个数据
    }
    val sData = Flow (Vec(Vec(UInt(config.DATA_WIDTH bits), config.DATA_NUM), config.DATA_NUM))
    sData.valid := io.sData.valid
    for(y <- -config.HALF_PATCH_SIZE to config.HALF_PATCH_SIZE){
        for (x <- -config.HALF_PATCH_SIZE to config.HALF_PATCH_SIZE) {
            val absy = abs(y);
            val absx = abs(x);
            if (config.umax(absy) < absx) {
                sData.payload(y + config.HALF_PATCH_SIZE)(x + config.HALF_PATCH_SIZE) := U(0 ,config.DATA_WIDTH bits)
            }
            else {
                sData.payload(y + config.HALF_PATCH_SIZE)(x + config.HALF_PATCH_SIZE) := io.sData.payload(y + config.HALF_PATCH_SIZE)(x + config.HALF_PATCH_SIZE)
            }
        }
    }

    //求和后使用dsp实现（d-a）*b + p这个运算。d和a还有b的输入都是有符号数，因此给出这个数据的时候需要高位填0变为无符号数分别位8 + 6和4 + 1的一个位宽得到的结果经过累加计算出最大可能的位宽然后截断。
    def addTree(dataIn:Vec[UInt]) : UInt = {//综合三个数和两个数的加法，节约资源。
        //第一次求和将31个数变为16位
        val add1 = Vec(UInt(dataIn(0).getWidth + 1 bits), (config.DATA_NUM + 1) / 2)//得到16位进位结果
        for(i <- 0 until  config.DATA_NUM / 2){//0到29求和
            add1(i) := dataIn(2 * i) +^ dataIn(2 * i + 1)
        }
        add1(config.DATA_NUM / 2) := dataIn(config.DATA_NUM - 1).resized
        //第二次求和将16个数变为6位
        val add2 = Vec(UInt(dataIn(0).getWidth + 3 bits), 6) //得到16位进位结果
        for (i <- 0 until 5) { //0到29求和
            add2(i) := add1(3 * i) +^ add1(3 * i + 1) +^ add1(3 * i + 2)
        }
        add2(5) := add1(config.DATA_NUM / 2).resized
        //第三次求和将6个数变为两位
        val add3 = Vec(UInt(dataIn(0).getWidth + 5 bits), 2) //得到16位进位结果
        for (i <- 0 until 2) { //0到29求和
            add3(i) := add2(3 * i) +^ add2(3 * i + 1) +^ add2(3 * i + 2)
        }
        //最后一次求和
        val sum = UInt(dataIn(0).getWidth + 5 bits) //得到16位进位结果

        add1.setAsReg() simPublic()
        add2.setAsReg() simPublic()
        add3.setAsReg() simPublic()
        //sum.setAsReg()
        //延迟三个周期
        sum := add3(0) + add3(1)
        sum
    }
    //（D-A）*B 输入第二个值开始进行累加，//得到的结果进行相加，相加后输出到下一个模块
    def subMulAccumulate(D: UInt, A:UInt, B:UInt, first: Bool, P: SInt){
        val compute = new icAngleMul(this.clockDomain)
        compute.io.D := D.resize(14).asSInt
        compute.io.A := A.resize(14).asSInt
        compute.io.B := B.resize(5).asSInt
        compute.io.SEL := first
        P := compute.io.P.resized
    }
    //P的范围是多少？ 0 - 20,第20位表示符号位，
    def rowOrColSumHlaf(dataIn1:Vec[UInt], dataIn2:Vec[UInt], B:UInt, first: Bool, P: SInt): Unit = {//经过若干个周期返回一个有效值
        val sum1 = addTree(dataIn1) simPublic()
        val sum2 = addTree(dataIn2) simPublic()
        val Acc = subMulAccumulate(sum2, sum1, B, first, P)
    }

    def bData(en: Bool, init:Int, endEn : Bool): UInt = {
        val count = UInt(4 bits) setAsReg() init(init)
        when(en){
            count := count - 2
            when(endEn){
                count:=init
            }
        }
        count
    }
    //这里包含延迟时间
    def control(B1:UInt, B2:UInt, sel:UInt, first:Bool, end:Bool): Unit = {//整个模块的控制模块
        //现在的valid是不对的需要延迟若干个周期与 乘法器配合
        val timeCount = WaCounter(sData.valid, 3, 7)//从0记录到7，必须保证valid信号有效的时间是8个周期，让timeCount能够恢复初始状态
        val DelayValid3 = Delay(sData.valid, 3, init = False)
        val DelayEnd3 = Delay(timeCount.valid, 3, init = False)
        B1 := bData(DelayValid3, 15, DelayEnd3)//记录到最后一个下周期恢复到初始状态
        B2 := bData(DelayValid3, 14, DelayEnd3)//14 12 10 8 6 4 2 0
        sel := timeCount.count
        first := Delay(!timeCount.count.orR, 3, init = False)
        end := DelayEnd3
    }
    //根据sData和计数器选择不太的数据给计算模块
    def selData(dataOut1:Vec[UInt], dataOut2:Vec[UInt], selCount:UInt, isRow:Boolean, isEdge:Boolean): Unit = {
        switch(selCount){
            for(i <- 0 to 7){
                is(i){
                    if(isRow){
                        if(isEdge){
                            dataOut1 := sData.payload(2 * i)
                            dataOut2 := sData.payload(30 - 2 * i)
                        } else {
                            dataOut1 := sData.payload(2 * i + 1)
                            dataOut2 := sData.payload(30 - 2 * i - 1)
                        }
                    } else {
                        if (isEdge) {
                            for(j <- 0 to 30){
                                dataOut1(j) := sData.payload(j)(2 * i)
                                dataOut2(j) := sData.payload(j)(30 - 2 * i)
                            }
                        } else {
                            for (j <- 0 to 30) {
                                dataOut1(j) := sData.payload(j)(2 * i + 1)
                                dataOut2(j) := sData.payload(j)(30 - 2 * i - 1)
                            }
                        }
                    }
                }
            }
        }
    }

    val sum = new Area {
        val P = Vec(SInt(20 bits), 4)
        val B1,B2 = UInt(4 bits)
        val sel = UInt(3 bits)
        val first,end = Bool()
        val SelColData = Vec(Vec(UInt(config.DATA_WIDTH bits), config.DATA_NUM), 4)
        val SelRowData = Vec(Vec(UInt(config.DATA_WIDTH bits), config.DATA_NUM), 4)
        control(B1, B2, sel, first, end)//控制信号
        selData(SelRowData(0), SelRowData(1), sel, true, true)//选择数据
        selData(SelRowData(2), SelRowData(3), sel, true, false)
        selData(SelColData(0), SelColData(1), sel, false, true)
        selData(SelColData(2), SelColData(3), sel, false, false)
        rowOrColSumHlaf(SelRowData(0), SelRowData(1), B1, !first, P(0))//后面的数据减前面的数据
        rowOrColSumHlaf(SelRowData(2), SelRowData(3), B2, !first, P(1))//后面的数据减前面的数据
        rowOrColSumHlaf(SelColData(0), SelColData(1), B1, !first, P(2))//后面的数据减前面的数据
        rowOrColSumHlaf(SelColData(2), SelColData(3), B2, !first, P(3))//后面的数据减前面的数据

        io.mData.payload(0) := P(0) + P(1)//m01 y 计算Row的部分
        io.mData.payload(1) := P(2) + P(3)//m10 x 计算Col的部分
        io.mData.valid := Delay(end, 4 + 1,init = False)
        io.mData.payload.setAsReg()
    }
}


object IC_Angle extends App {
    SpinalVerilog(new IC_Angle(IC_AngleConfig()))
}