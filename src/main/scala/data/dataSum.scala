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
    val done = in Bool()
    val sDataRsBrief = slave Stream Bits(64 bits)
    val mdata = master Stream new OrbFpRsIO(SIZE_WIDTH, DATA_WIDTH)
    val mdataLast = out Bool()
    val flush = in Bool()
    val topNum = in UInt(16 bits)

    val inputLength = out(UInt(16 bits))
    val outputLength = out(UInt(16 bits))
  }
  val done = RegInit(False) setWhen(io.done) clearWhen(io.flush)
  //完成信号
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
    //已经存在数据，那么需要等待后续还有数据，如果后续不存在数据那么最后一个数据等待done信号
    io.mdata <> fpRs.continueWhen(fifo.io.pop.valid || done)
    io.mdataLast := done
    val outputCnt = WaCounter(io.mdata.fire, 16, (1 << 16) - 1) //记录输出数据个数
    io.outputLength := outputCnt.count
    io.inputLength := outputCnt.count
    when(io.flush){
      outputCnt.clear
    }
  } else{
    def compare(left:OrbFpRsIO, right:OrbFpRsIO):Bool = {
      left.fp.score > right.fp.score
    }

    val sortDrop = new SortDrop(fpRs.payload, TopSort, compare)
    sortDrop.io.flush := io.flush
    sortDrop.io.sData <> fpRs
    sortDrop.io.done := io.done
    io.outputLength := sortDrop.io.outputLength
    io.inputLength := sortDrop.io.inputLength
    io.mdataLast := sortDrop.io.outputLast
    sortDrop.io.mData <> io.mdata
    sortDrop.io.topNum := io.topNum
    sortDrop.io.defaultData.fp.size.rowNum := U(0).resized
    sortDrop.io.defaultData.fp.size.colNum := U(0).resized
    sortDrop.io.defaultData.fp.score := U(0).resized
    sortDrop.io.defaultData.rs := B(0).resized
  }
}

object DataSum extends App {
  SpinalVerilog(new DataSum(11, 8, 64)).printPruned
}