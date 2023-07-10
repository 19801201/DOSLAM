package dataStructure

import operator.NMSConfig
import spinal.lib.experimental.chisel.Bundle
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import spinal.lib.experimental.chisel.Module

case class FeaturePoint(config : NMSConfig) extends Bundle{
    val colNum = UInt(config.SIZE_WIDTH - 3 bits)
    val selNum = UInt(3 bits)
    val rowNum = UInt(config.SIZE_WIDTH bits)
    val dataPoint = UInt(config.DATA_WIDTH bits)
    val num = UInt(2 bits)
}
