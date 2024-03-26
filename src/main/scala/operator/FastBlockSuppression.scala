package operator

import data.{ReflectionFillWindow, ReflectionFillWindowConfig, WindowsConfig, formatCornerScoreWindos, syncWindowsPadding, syncWindowsPadding2}
import dataStructure.{FeaturePoint, FeaturePointOrb}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}

import java.io.Flushable

case class FastBlockSuppressionConfig(DATA_NUM : Int = 8,
                                      MEM_DEPTH : Int = 1024,
                                      SIZE_WIDTH : Int = 11,
                                      BSNum:Int = 3,
                                      isBlock : Boolean = false){
  val DATA_WIDTH = 8
  val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
  val ADDR_WIDTH = log2Up(MEM_DEPTH)
}

class FastBlockSuppression(config:FastBlockSuppressionConfig) extends Module {
  val io = new Bundle { //给出输入得到输出结果
    //增加运行速度，一次传输多个个数据
    val sData = slave Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH + 2)
    val mData = master Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH)

    val start = in Bool()
    val done = in Bool()
  }
  //col/64 row/16 拼接为地址数据
  def addressTranslation(cnt:ImageSize): UInt ={
    (cnt.colNum >> 6) +^ ((cnt.rowNum >> 4) << 4)
  }
  val fsm = new StateMachine {
    setEncoding(binaryOneHot)
    val IDLE = new State with EntryPoint
    val VALID = new State
    val FLUSH = new State
    val DONE = new State
    //两种情况下会输出有效数据，第一种接收到有效数据，第二种mready为真，输出有效数据
    val flushCnt = WaCounter(isActive(FLUSH), 11, config.MEM_DEPTH)
    IDLE
      .whenIsActive {
        when(io.start.rise()) {
          goto(VALID)
        }
      }
    FLUSH
      .whenIsActive{
        when(flushCnt.valid){
          goto(VALID)
        }
      }
    VALID
      .whenIsActive { //行列都为0
        when(io.done.rise()) {
          goto(DONE)
        }
      }
    DONE
      .whenIsActive{
        when
      }
  }

  val rdAddr = addressTranslation(io.sData.payload.size).resize(config.ADDR_WIDTH)
  val wrAddr = RegNext(rdAddr)
  //判断是否需要重新取数据,第一个点一定需要重新取数据，其余情况下数据不相同才需要重新取数据


  val mem = Mem(HardType(UInt(4 bits)), wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")
  //数据读出
  val rdMemCnt = mem.readSync(rdAddr, io.sData.fire && io.sData.payload.score(8))
  val rdMemCntWriteFirst = UInt(4 bits)
  val addCount = (!rdMemCnt.andR).mux(rdMemCntWriteFirst + 1, rdMemCntWriteFirst)

  //数据写入
  val writeAddr = fsm.isActive(fsm.VALID).mux(wrAddr, fsm.flushCnt.count.resized)
  val writeEn = fsm.isActive(fsm.FLUSH) || (fsm.isActive(fsm.VALID) && RegNext(io.sData.fire,False) && RegNext(io.sData.payload.score(8)))
  val writeData = fsm.isActive(fsm.VALID).mux(addCount ,U(0, 4 bits))
  mem.write(writeAddr, writeData, writeEn) //同步写,使能延迟
  //延迟一个周期后，读出
  rdMemCntWriteFirst := (writeEn && (io.sData.fire && io.sData.payload.score(8)) && (rdAddr === writeAddr)).mux(RegNext(writeData), rdMemCnt)
  //如果读写相同地址那么读到的数据应该是写入的数据

  val fifo = new StreamFifo(io.sData.payload, config.MEM_DEPTH)

  //计算最新的数据
  val rowCnt = RegNext(io.sData.payload.size.rowNum >> 4)
  //数据位置有效
  val dataPosValid = (fsm.isActive(fsm.VALID) && (fifo.io.pop.payload.size.rowNum(config.SIZE_WIDTH - 1 downto 4) < rowCnt)) || fsm.isActive(fsm.DONE)


  fifo.io.push <> io.sData
  val popDataRegNext = fifo.io.pop.continueWhen(dataPosValid).m2sPipe()
  //数据是否被刷新
  val rdfphTiming = mem.readSync(addressTranslation(fifo.io.pop.payload.size).resize(config.ADDR_WIDTH), fifo.io.pop.fire)
  val dataTexValid = ((rdfphTiming >= config.BSNum) && popDataRegNext.payload.score(9) && popDataRegNext.payload.score(8)) || ((rdfphTiming < config.BSNum))

//  val adwdawa =
  when(dataTexValid) {
    popDataRegNext.ready := True
    io.mData.valid := False
  } otherwise {
    popDataRegNext.ready := io.mData.ready
    io.mData.valid := popDataRegNext.valid
  }
  io.mData.size := popDataRegNext.payload.size
  io.mData.score := popDataRegNext.payload.score(config.DATA_WIDTH - 1 downto 0)
  //判断数据是否可以正常传出
}

object FastBlockSuppression extends App {
  SpinalVerilog(new FastBlockSuppression(FastBlockSuppressionConfig())).printPruned
}