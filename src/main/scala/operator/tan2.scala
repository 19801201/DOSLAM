package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import wa.ip._
import spinal.lib.experimental.chisel.Module
import spinal.core.sim._
case class tan2Config(DATA_NUM : Int = 2,
                          MEM_DEPTH : Int = 1024,
                          SIZE_WIDTH : Int = 11
                         ) {
    val WINDOWS_SIZE_H = 7
    val DATA_WIDTH = 20
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    val TAN_VALUE_Q = Array(3259, 6786, 10947, 16384, 24520, 39554, 82368);//对7个tan值进行量化，得到结果
    //创建的MEM个数
}
//先给x后给y
class tan2(config:tan2Config) extends Module{
    val io = new Bundle {//要求valid有效之后数据需要保持8周期的有效值//
        val sData = slave Flow Vec(SInt(config.DATA_WIDTH bits), config.DATA_NUM)//得到质心位置，求出角度 1 x 0 y
        val mData = master Flow UInt(5 bits) //求出选择角度
        val debugge,debugValidDelay4,debugrecordValid = out Bool()
    }
    /*
    + + ：A*B-C
    + - ：A*B+C
    - + ：(0-A)*B-C
    - - ：C-A*B
     */
    def MulCompare(A: SInt, B: UInt, C: SInt, sel: Bits, GE: Bool) {
        val compute = new tan2Mul(this.clockDomain)
        compute.io.D := 0
        compute.io.A := A//x
        compute.io.B := B.resize(B.getWidth + 1).asSInt//mul
        compute.io.C := (C @@ U(0, 14 bits)).resized
        compute.io.SEL := sel
        GE := !compute.io.P.sign//如果符号位为0，说明结果大于等于，否则说明结果小于
    }

    def control(start: Bool, B: UInt, valid:Bool): UInt = {
        val comState = Reg(Bool()).setWhen(start)
        val count = WaCounter(start||comState, 3, 6)
        comState.clearWhen(count.valid)
        switch(count.count){
            for(i <- 0 until 7){
                is(i){
                    B := U(config.TAN_VALUE_Q(i), B.getWidth bits)
                }
            }
            default{
                B := 0
            }
        }
        valid := Delay(count.count.orR || start, 4)
        Delay(count.count, 4)
    }

    def inputCompute(selTan:UInt): Unit = {//处理输入和计算 这里需要给出
        val sDataReg = RegNextWhen(io.sData.payload, io.sData.valid)
        sDataReg.foreach(_ init(0))
        val B = UInt(17 bits)
        val A, C = SInt(20 bits)
        val sel = Bits(2 bits)
        val GE, ValidDelay4 = Bool()
        io.debugge:=GE
        io.debugValidDelay4 := ValidDelay4
        val curSelTan = control(io.sData.valid, B, ValidDelay4)//同步给出有效信号和选择信号
        when(io.sData.valid) {
            A := io.sData.payload(1)
            C := io.sData.payload(0)
            sel := io.sData.payload(1).sign ## io.sData.payload(0).sign
        } otherwise {
            A := sDataReg(1)
            C := sDataReg(0)
            sel := sDataReg(1).sign ## sDataReg(0).sign
        }
        MulCompare(A, B, C, sel, GE) //在这里得到比较结果 根据得到的结果找到合适的值
        //记录本次处理
        val recordValid = Reg(Bool) init False
        io.debugrecordValid := recordValid
        when(GE && ValidDelay4){//当需要更新时，并且数据有效，那么记录本次更新
            recordValid := True
        } elsewhen(io.mData.valid){
            recordValid := False
        }
        val recordTan = Reg(UInt(3 bits)) init(7);//默认值是7，如果没有条件满足那么这个值就是7
        when(GE && ValidDelay4 && !recordValid) {
            recordTan := curSelTan
        } elsewhen (io.mData.valid) {//数据到达第7+4个周期开始有效
            recordTan := 7
        }
        selTan := recordTan
        //选择的领域
    }


    val sum = new Area {
        io.mData.valid := Delay(io.sData.valid, 7 + 4)//计算延迟4个周期 等待7个周期的计算
        val quadrant = Reg(UInt(2 bits)) init 0
        when(io.sData.valid){//记录数据在那个象限内
            when(io.sData.payload(1) > 0 && io.sData.payload(0) >= 0){
                quadrant := 0
            } elsewhen(io.sData.payload(1) <= 0 && io.sData.payload(0) > 0){
                quadrant := 1
            } elsewhen (io.sData.payload(1) < 0 && io.sData.payload(0) <= 0) {
                quadrant := 2
            } otherwise{
                quadrant := 3
            }
        }
        val selTan = UInt(3 bits)
        inputCompute(selTan)//得到结果
        switch(Delay(quadrant, 4)){//根据输入所在的象限得到最终结果，数据本身会延迟8个周期，因此再这个基础上再延迟4个周期确保数据输出时，一定处于有效位置
            is(0){
                io.mData.payload := selTan.resized
            }
            is(1){
                io.mData.payload := U(15, 5 bits) - selTan.resized
            }
            is(2) {
                io.mData.payload := U(16, 5 bits) + selTan.resized
            }
            is(3) {
                io.mData.payload := U(31, 5 bits) - selTan.resized
            }
        }
    }
}


object tan2 extends App {
    SpinalVerilog(new tan2(tan2Config()))
}