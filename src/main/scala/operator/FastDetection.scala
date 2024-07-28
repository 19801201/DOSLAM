package operator
import spinal.core.sim._
import spinal.core._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

case class FastDetectionConfig(DATA_WIDTH : Int = 8,
                                     DATA_NUM : Int = 17,
                                     DATA_W : Int = 3 + 8 + 3,
                                     DATA_H :Int = 7
                                    ){
    val x = Array(0, 1, 2, 3, 3, 3, 2, 1, 0, -1, -2, -3, -3, -3, -2, -1, 0, 1, 2, 3, 3, 3, 2, 1, 0)
    val y = Array(3, 3, 2, 1, 0, -1, -2, -3, -3, -3,-2 ,-1, 0, 1, 2, 3, 3, 3, 2, 1, 0, -1, -2, -3,-3)
}
//这个函数需要尽快出结果，最好只使用一两个周期。
class FastDetection(fastConfig : FastDetectionConfig) extends Component {
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Flow (Vec(Bits(fastConfig.DATA_WIDTH bits), fastConfig.DATA_NUM)) //第一个是得分点,其余的是周围的点
        val mData = master Flow Bits(2 bits)
        val threshold = in UInt(fastConfig.DATA_WIDTH bits)
    }//两级流水，对输入fast做特征点检测，得到的暗点存入第一个Vec中，得到的第二个亮点存入第二个Vec中

    //一级流水 接收数据 计算亮的点和暗的点
    val vtDark = SInt(fastConfig.DATA_WIDTH + 1 bits)
    val vtLight = UInt(fastConfig.DATA_WIDTH + 1 bits)
    vtDark := (B"1'b0" ## io.sData.payload(0)).asSInt - (U"1'b0" ##io.threshold).asSInt
    vtLight := (B"1'b0" ## io.sData.payload(0)).asUInt + (U"1'b0" ##io.threshold).asUInt
    val sDataDelay = Vec(Bits(fastConfig.DATA_WIDTH bits), fastConfig.DATA_NUM)
    sDataDelay := io.sData.payload
    //二级流水 -- 计算大小的值 资源消耗8 * 16 * 2个9位比较器
    val lightPoint, darkPoint = Vec(Bool(), 16)
    for(j <- 0 to 15){//遍历周围的16个点分别计算比阈值更亮的点和比阈值更暗的点
        lightPoint(j) := sDataDelay(j + 1).asUInt > vtLight
        darkPoint(j) := (B"1'b0" ## sDataDelay(j+1)).asSInt < vtDark
    }

    //三级流水 -- 计算亮点和暗点
    val lightCount2, darkCount2  = Vec(Bool(), 8)
    val lightCount4, darkCount4  = Vec(Bool(), 8)
    val lightCount8, darkCount8  = Vec(Bool(), 8)
    val lightCount9, darkCount9  = Vec(Bool(), 8)
    val light = Bool()
    val dark = Bool()
    for (j <- 0 until 8) { //遍历周围的16个点分别计算比阈值更亮的点和比阈值更暗的点
        lightCount2(j) := lightPoint(2 * j) & lightPoint((2 * j + 1) % 16)
        darkCount2(j) := darkPoint(2 * j) & darkPoint((2 * j + 1) % 16)
        lightCount4(j) := lightCount2(j) & lightCount2((j + 1) % 8)
        darkCount4(j) := darkCount2(j) & darkCount2((j + 1) % 8)
        lightCount8(j) := lightCount4(j) & lightCount4((j + 2) % 8)
        darkCount8(j) := darkCount4(j) & darkCount4((j + 2) % 8)
        lightCount9(j) := lightCount8(j) & Delay((lightPoint((2 * j + 8) % 16) || lightPoint((2 * j + 15) % 16)),1)
        darkCount9(j) := darkCount8(j) & Delay((darkPoint((2 * j + 8) % 16) || darkPoint((2 * j + 15) % 16)),1)
    }
    light := lightCount9.orR
    dark := darkCount9.orR
    io.mData.payload(0) := light
    io.mData.payload(1) := dark
    //io.mData.valid := Delay(io.sData.valid, 1) && (light || dark)
    io.mData.valid := Delay(io.sData.valid, 1, init = False)
    //io.mData.payload.setAsReg()
    lightCount4.setAsReg()
    darkCount4.setAsReg()
}

object FastDetection extends App {
    SpinalVerilog(new FastDetection(FastDetectionConfig()))
}