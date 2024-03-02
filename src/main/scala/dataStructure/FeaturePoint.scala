package dataStructure

import operator.NMSConfig
import spinal.lib.experimental.chisel.Bundle
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import spinal.lib.experimental.chisel.Module
import utils.ImageSize
/*
    colNum:列数
    selNum:列的第n个数据
    rowNum:行数
    dataPoint:得分值

 */
case class FeaturePoint(config : NMSConfig) extends Bundle{
    val colNum = UInt(config.SIZE_WIDTH - 3 bits)
    val selNum = UInt(3 bits)
    val rowNum = UInt(config.SIZE_WIDTH bits)
    val dataPoint = UInt(config.DATA_WIDTH bits)
    val num = UInt(2 bits)
}
//with IMasterSlave
class FeaturePointOrb(SIZE_WIDTH : Int, DATA_WIDTH : Int) extends Bundle {
    val size = new ImageSize(SIZE_WIDTH)
    val score = UInt(DATA_WIDTH bits)

    def this(SIZE_WIDTH: Int, DATA_WIDTH: Int, size: ImageSize, score: UInt) {
        this(SIZE_WIDTH, DATA_WIDTH)
        this.size := size
        this.score := score
    }

//    override def asMaster(): Unit = {
//        out(size, score)
//    }
}

case class BfMatchIO(indxWidth:Int = 12, distance:Int = 8) extends Bundle with IMasterSlave {
    val index = Vec(UInt(indxWidth bits), 2)
    val distant = UInt(distance bits)

    override def asMaster(): Unit = {
        out(index, distant)
    }

    def set(distant : UInt) {
        this.distant := distant
    }
}