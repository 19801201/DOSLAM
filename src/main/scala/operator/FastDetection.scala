package operator
import spinal.core.sim._
import spinal.core._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

case class FastDetectionConfig(DATA_WIDTH : Int = 8,
                                     DATA_NUM : Int = 8,
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
        val sData = slave Flow (Vec(Vec(Bits(fastConfig.DATA_WIDTH bits), fastConfig.DATA_W), fastConfig.DATA_H))
        val mData = master Flow (Vec(Vec(Bool(), fastConfig.DATA_NUM), 2))
        val threshold = in UInt(fastConfig.DATA_WIDTH bits)
    }//两级流水，对输入fast做特征点检测，得到的暗点存入第一个Vec中，得到的第二个亮点存入第二个Vec中

    //一级流水 接收数据 计算亮的点和暗的点
    val vtDark = Vec(SInt(fastConfig.DATA_WIDTH + 1 bits), fastConfig.DATA_NUM)
    val vtLight = Vec(UInt(fastConfig.DATA_WIDTH + 1 bits), fastConfig.DATA_NUM)
    for (i <- 0 to 7) { //首先进行计算亮和暗值,根据亮值和暗值来计算。
        vtDark(i) := (B"1'b0" ## io.sData.payload(3)(3 + i)).asSInt - (U"1'b0" ##io.threshold).asSInt
        vtLight(i) := (B"1'b0" ## io.sData.payload(3)(3 + i)).asUInt + (U"1'b0" ##io.threshold).asUInt
    }
    val sDataDelay = Vec(Vec(Bits(fastConfig.DATA_WIDTH bits), fastConfig.DATA_W), fastConfig.DATA_H).setAsReg()
    sDataDelay := io.sData.payload
    //二级流水 -- 计算大小的值 资源消耗8 * 16 * 2个9位比较器
    val lightPoint, darkPoint = Vec(Vec(Bool().setAsReg(), 16), fastConfig.DATA_NUM)
    for(i <- 0 until  fastConfig.DATA_NUM){//对于每个FAST特征点来说
        for(j <- 0 to 15){//遍历周围的16个点分别计算比阈值更亮的点和比阈值更暗的点
            lightPoint(i)(j) := sDataDelay(3 + fastConfig.y(j))(i + 3 + fastConfig.x(j)).asUInt > vtLight(i)
            darkPoint(i)(j) := (B"1'b0" ## sDataDelay(3 + fastConfig.y(j))(i + 3 + fastConfig.x(j))).asSInt < vtDark(i)
        }
    }

    //三级流水 -- 计算亮点和暗点
    val lightCount2, darkCount2  = Vec(Vec(Bool(), 16), fastConfig.DATA_NUM)
    val lightCount4, darkCount4  = Vec(Vec(Bool(), 16), fastConfig.DATA_NUM)
    val lightCount8, darkCount8  = Vec(Vec(Bool(), 16), fastConfig.DATA_NUM)
    val lightCount9, darkCount9  = Vec(Vec(Bool(), 16), fastConfig.DATA_NUM)
    for (i <- 0 until fastConfig.DATA_NUM) { //对于每个FAST特征点来说
        for (j <- 0 to 15) { //遍历周围的16个点分别计算比阈值更亮的点和比阈值更暗的点
            lightCount2(i)(j) := lightPoint(i)(j) & lightPoint(i)((j + 1) % 16)
            darkCount2(i)(j) := darkPoint(i)(j) & darkPoint(i)((j + 1) % 16)
            lightCount4(i)(j) := lightCount2(i)(j) & lightCount2(i)((j + 2) % 16)
            darkCount4(i)(j) := darkCount2(i)(j) & darkCount2(i)((j + 2) % 16)
            lightCount8(i)(j) := lightCount4(i)(j) & lightCount4(i)((j + 4) % 16)
            darkCount8(i)(j) := darkCount4(i)(j) & darkCount4(i)((j + 4) % 16)
            lightCount9(i)(j) := lightCount8(i)(j) & lightCount8(i)((j + 1) % 16)
            darkCount9(i)(j) := darkCount8(i)(j) & darkCount8(i)((j + 1) % 16)
        }
        io.mData.payload(0)(i) := lightCount9(i).asBits.orR
        io.mData.payload(1)(i) := darkCount9(i).asBits.orR
    }
    io.mData.valid := Delay(io.sData.valid, 1)
    //io.mData.payload.setAsReg()
}

object FastDetection extends App {
    SpinalVerilog(new FastDetection(FastDetectionConfig()))
}