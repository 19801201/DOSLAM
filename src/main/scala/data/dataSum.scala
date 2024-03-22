package data

import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import data._
import dataStructure.{FeaturePoint, FeaturePointOrb, OrbFpRsIO}
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}
import spinal.lib.experimental.chisel.Module

class DataSum (SIZE_WIDTH:Int, DATA_WIDTH:Int, TopSort:Int = -1) extends Module{
  val io = new Bundle{
    val sData = slave Flow new FeaturePointOrb(SIZE_WIDTH, DATA_WIDTH)
    val sDataRsBrief = slave Stream Bits(64 bits)
    val mdata = master Stream new OrbFpRsIO(SIZE_WIDTH, DATA_WIDTH)
  }
  val fifo = new StreamFifo(io.sData.payload, 128)
  fifo.io.push.payload := io.sData.payload
  fifo.io.push.valid := io.sData.fire

  val validCnt = WaCounter(fifo.io.pop.fire || io.sDataRsBrief.fire, 3, 4)

  val fpRs = Stream(new OrbFpRsIO(SIZE_WIDTH, DATA_WIDTH))

  fifo.io.pop.ready := (!validCnt.count.orR) && (fpRs.ready || !fpRs.valid)
  io.sDataRsBrief.ready := (validCnt.count.orR) && (fpRs.ready || !fpRs.valid)

  fpRs.payload.setAsReg()
  fpRs.valid.setAsReg() init(False)
  for(i <- 0 until 5){
    switch(validCnt.count){
      if(i == 0){
        is(0){
          when(fifo.io.pop.fire){
            fpRs.fp := fifo.io.pop.payload
          }
        }
      } else {
        is(i){
          when(io.sDataRsBrief.fire){
            fpRs.rs.subdivideIn(4 slices)(i - 1) := io.sDataRsBrief.payload
          }
        }
      }
    }
  }
  fpRs.valid clearWhen (fpRs.ready) setWhen(validCnt.validLast())
  if(TopSort <= 0){
    io.mdata <> fpRs
  }
}

object DataSum extends App {
  SpinalVerilog(new DataSum(11, 8)).printPruned
}