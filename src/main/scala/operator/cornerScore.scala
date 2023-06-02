package operator

import spinal.core.sim._
import spinal.core._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

case class cornerScoreConfig(DATA_WIDTH : Int = 8,
                      DATA_NUM : Int = 16 + 1,
                      DATA_W : Int = 3 + 8 + 3,
                      DATA_H :Int = 7
                     ){
}
/*
一次只处理一个数据，传入一个点和
 */
class CornerScore(scoreConfig : cornerScoreConfig) extends Component {
    val io = new Bundle {//输出一个分数
        //增加运行速度，一次传输多个个数据
        val sData = slave Flow (Vec(Bits(scoreConfig.DATA_WIDTH bits), scoreConfig.DATA_NUM)) //第一个是得分点,其余的是周围的点
        val islight = in Bool()
        val mData = master Flow Bits(scoreConfig.DATA_WIDTH bits)
    }//两级流水，对输入fast做特征点检测，得到的暗点存入第一个Vec中，得到的第二个亮点存入第二个Vec中

    //输入模块计算
    val d = Vec(SInt(scoreConfig.DATA_WIDTH + 1 bits), scoreConfig.DATA_NUM - 1)
    val d_a = Vec(SInt(scoreConfig.DATA_WIDTH + 1 bits), scoreConfig.DATA_NUM - 1)
    val d_b = Vec(SInt(scoreConfig.DATA_WIDTH + 1 bits), scoreConfig.DATA_NUM - 1)
    val d_out = Vec(UInt(scoreConfig.DATA_WIDTH bits).setAsReg(), scoreConfig.DATA_NUM - 1)
    d_a.foreach((elem : SInt) => when(io.islight){ elem := (B"1'b0" ## io.sData.payload(0)).asSInt} otherwise { elem := (B"1'b0" ## io.sData.payload(d_a.indexOf(elem) + 1)).asSInt })
    d_b.foreach((elem : SInt) => when(io.islight){ elem := (B"1'b0" ## io.sData.payload(d_a.indexOf(elem) + 1)).asSInt} otherwise { elem := (B"1'b0" ## io.sData.payload(0)).asSInt })
    d.foreach((elem : SInt) => elem := d_a(d.indexOf(elem)) - d_b(d.indexOf(elem)))
    d_out.foreach((elem : UInt) => when(d(d_out.indexOf(elem))(scoreConfig.DATA_WIDTH) === True){elem := U"8'b0"} otherwise{elem := d(d_out.indexOf(elem)).asUInt.resized})

    def min(res: Vec[UInt], input: Vec[UInt], mul: Int, index: Int): Unit = {
        res.foreach((elem: UInt) => when(input((res.indexOf(elem) * mul) % input.length) > input((res.indexOf(elem) * mul + index) % input.length)) {
            elem := input((res.indexOf(elem) * mul + index) % input.length)
        } otherwise {
            elem := input((res.indexOf(elem) * mul) % input.length)
        })
    }

    def max(res: Vec[UInt], input: Vec[UInt], mul: Int, index: Int): Unit = {
        res.foreach((elem: UInt) => when(input((res.indexOf(elem) * mul) % input.length) < input((res.indexOf(elem) * mul + index) % input.length)) {
            elem := input((res.indexOf(elem) * mul + index) % input.length)
        } otherwise {
            elem := input((res.indexOf(elem) * mul) % input.length)
        })
    }

    //一级流水
    val max2d =Vec(UInt(scoreConfig.DATA_WIDTH bits).setAsReg(), (scoreConfig.DATA_NUM - 1)/2)
    val min2 = Vec(UInt(scoreConfig.DATA_WIDTH bits), (scoreConfig.DATA_NUM - 1)/2)
    val min4 = Vec(UInt(scoreConfig.DATA_WIDTH bits).setAsReg(), (scoreConfig.DATA_NUM - 1)/2)
    val min8 = Vec(UInt(scoreConfig.DATA_WIDTH bits), (scoreConfig.DATA_NUM - 1)/2)
    val max2 = Vec(UInt(scoreConfig.DATA_WIDTH bits).setAsReg(), (scoreConfig.DATA_NUM - 1)/2)
    val max4 = Vec(UInt(scoreConfig.DATA_WIDTH bits), (scoreConfig.DATA_NUM - 1)/4)
    val max8 = Vec(UInt(scoreConfig.DATA_WIDTH bits).setAsReg(), (scoreConfig.DATA_NUM - 1)/8)
    val max16= Vec(UInt(scoreConfig.DATA_WIDTH bits), (scoreConfig.DATA_NUM - 1)/16)
    max2d.foreach((elem: UInt) => when(d_out((15 + 2 * max2d.indexOf(elem)) % 16) > d_out((8 + 2 * max2d.indexOf(elem)) % 16)){elem := d_out((15 + 2 * max2d.indexOf(elem)) % 16)} otherwise {elem := d_out((8 + 2 * max2d.indexOf(elem)) % 16)})
    min(min2, d_out, 2, 1)
    min(min4, min2, 1, 1)
    min(min8, min4,1,2)
    max2.foreach((elem: UInt) => when(min8(max2.indexOf(elem)) < max2d(max2.indexOf(elem))){elem := min8(max2.indexOf(elem))} otherwise {elem := max2d(max2.indexOf(elem))} )
    max(max4, max2, 2, 1)
    max(max8, max4, 2, 1)
    max(max16, max8,2, 1)
    io.mData.payload := max16(0).asBits.resized
    io.mData.valid := Delay(io.sData.valid,3)
}

object CornerScore extends App {
    SpinalVerilog(new CornerScore(cornerScoreConfig()))
}