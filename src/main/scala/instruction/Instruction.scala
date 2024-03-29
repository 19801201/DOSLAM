package instruction

import config.Config._
import move.{DMA_CONFIG, axis_desc}
import spinal.core.{Vec, _}
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif._
/*
  val io = new Bundle { //给出输入得到输出结果
    //数据通道
    val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)//图片输入
    val mDataImage = master Stream Bits(config.DATA_STREAM_WIDTH bits)//缩放后的图片输出
    val mData = master Stream new OrbFpRsIO(config.SIZE_WIDTH, config.DATA_WIDTH)
    val mDataLast = out(Bool())
    //输入信号和输出信号，确保size*size个数据同时输出
    val start = in Bool()//开始信号
    //开始信号
    //输入尺寸
    //全部的尺寸
    val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))//输入信号
    val threshold = in UInt (config.DATA_WIDTH bits)//阈值
    val maskF = in Bits(16 bits)//标志
    val maskR = in Bits(8 bits)
    val maskG = in Bits(8 bits)

    //经过位宽转换后的行和列
    //val rowNumFlilterIn = in UInt (config.SIZE_WIDTH bits)//输入行数为rowNumIn+1
    val colNumFlilterIn = in UInt (config.SIZE_WIDTH - 3 bits)//输入列数为colNumIn + 1,每次包含八列数字，最后一列可能包含无效数据
    //经过列无效筛选后的行和列
    val rowNumSrcIn = in UInt (config.SIZE_WIDTH bits) //窗口的输入大小。输入数据的个数不管到底有多少
    val colNumSrcIn = in UInt (config.SIZE_WIDTH bits) //输出数据的大小由上位机给出即可，完全不考虑边界问题。即输入即输出数据量

    val inValid = in Bits (3 bits)

    val inputLength = out(UInt(16 bits))
    val outputLength = out(UInt(16 bits))

    val topNum = in(UInt(16 bits))

    val thresholdInit = if(config.isBlock) (in UInt (config.DATA_WIDTH bits)) else null
  }
 */

class Instruction(configDmaRead:DMA_CONFIG, configDmaWriteImage:DMA_CONFIG,configDmaWriteOrb:DMA_CONFIG) extends Component {
  val io = new Bundle {
    val regSData = slave(AxiLite4(log2Up(1 MiB), 32))
    val ORBInstruction = out Vec(Reg(Bits(32 bits)) init 0, 12)
    val dmaImageRead = master Stream(axis_desc(configDmaRead))
    val dmaImageWrite = master Stream(axis_desc(configDmaWriteImage))
    val dmaOrbWrite = master Stream(axis_desc(configDmaWriteOrb))
    val ORBInstructionIn = in Vec(Bits(32 bits), 2)
  }
  noIoPrefix()
  //    AxiLite4SpecRenamer(io.regSData)
  val bus = BusInterface(io.regSData, sizeMap = SizeMapping(0, 1 MiB))
  //1、orb特征点
  val start = bus.newReg("start")
  val sizeInRow = bus.newReg("sizeInRow")
  val sizeInCol = bus.newReg("sizeInCol")
  val threshold   = bus.newReg("threshold")
  val maskF   = bus.newReg("maskF")           // start
  val maskR = bus.newReg("maskR")         // sel now
  val maskG     = bus.newReg("maskG")
  val colNumFlilterIn     = bus.newReg("colNumFlilterIn")
  val rowNumSrcIn    = bus.newReg("rowNumSrcIn")
  val inValid = bus.newReg("inValid")
  val topNum = bus.newReg("topNum")
  val thresholdInit = bus.newReg("thresholdInit")

  val inputLength = bus.newReg("inputLength")
  val outputLength = bus.newReg("outputLength")

  io.ORBInstruction(0) := start.field(Bits(1 bits), WO, doc = "start").resized
  io.ORBInstruction(1) := sizeInRow.field(Bits(32 bits), WO, doc = "sizeInRow")
  io.ORBInstruction(2) := sizeInCol.field(Bits(32 bits), WO, doc = "sizeInCol")
  io.ORBInstruction(3) := threshold.field(Bits(32 bits), WO, doc = "threshold")

  io.ORBInstruction(4) := maskF.field(Bits(32 bits), WO, doc = "maskF")
  io.ORBInstruction(5) := maskR.field(Bits(32 bits), WO, doc = "maskR")
  io.ORBInstruction(6) := maskG.field(Bits(32 bits), WO, doc = "maskG")
  io.ORBInstruction(7) := colNumFlilterIn.field(Bits(32 bits), WO, doc = "colNumFlilterIn")
  io.ORBInstruction(8) := rowNumSrcIn.field(Bits(32 bits), WO, doc = "rowNumSrcIn")

  io.ORBInstruction(9) := inValid.field(Bits(32 bits), WO, doc = "inValid")
  io.ORBInstruction(10) := topNum.field(Bits(32 bits), WO, doc = "topNum")
  io.ORBInstruction(11) := thresholdInit.field(Bits(32 bits), WO, doc = "thresholdInit")

  inputLength.field(Bits(32 bits), RO, doc = "inputLength") := io.ORBInstructionIn(0)
  outputLength.field(Bits(32 bits), RO, doc = "outputLength") := io.ORBInstructionIn(0)

  //数据读入
  val dmaImageReadValid = bus.newReg("dmaImageReadValid")
  val dmaImageReadReady = bus.newReg("dmaImageReadReady")
  val dmaImageReadAddr  = bus.newReg("dmaImageReadAddr")
  val dmaImageReadLen   = bus.newReg("dmaImageReadLen")

  io.dmaImageRead.valid := dmaImageReadValid.field(Bits(1 bits), WO, doc = "Valid").asBool
  dmaImageReadReady.field(Bits(1 bits), RO, doc = "Ready") := io.dmaImageRead.ready.asBits
  io.dmaImageRead.addr := dmaImageReadAddr.field(Bits(32 bits), WO, doc = "Addr").asUInt
  io.dmaImageRead.len := dmaImageReadLen.field(Bits(32 bits), WO, doc = "Len").asUInt.resized

  //图片读出
  val dmaImageWriteValid = bus.newReg("dmaImageWriteValid")
  val dmaImageWriteReady = bus.newReg("dmaImageWriteReady")
  val dmaImageWriteAddr  = bus.newReg("dmaImageWriteAddr")
  val dmaImageWriteLen   = bus.newReg("dmaImageWriteLen")

  io.dmaImageWrite.valid := dmaImageWriteValid.field(Bits(1 bits), WO, doc = "Valid").asBool
  dmaImageWriteReady.field(Bits(1 bits), RO, doc = "Ready") := io.dmaImageWrite.ready.asBits
  io.dmaImageWrite.addr := dmaImageWriteAddr.field(Bits(32 bits), WO, doc = "Addr").asUInt
  io.dmaImageWrite.len := dmaImageWriteLen.field(Bits(32 bits), WO, doc = "Len").asUInt.resized

  //特征点读出
  val dmaOrbWriteValid = bus.newReg("dmaOrbWriteValid")
  val dmaOrbWriteReady = bus.newReg("dmaOrbWriteReady")
  val dmaOrbWriteAddr  = bus.newReg("dmaOrbWriteAddr")
  val dmaOrbWriteLen   = bus.newReg("dmaOrbWriteLen")

  io.dmaOrbWrite.valid := dmaOrbWriteValid.field(Bits(1 bits), WO, doc = "Valid").asBool
  dmaOrbWriteReady.field(Bits(1 bits), RO, doc = "Ready") := io.dmaOrbWrite.ready.asBits
  io.dmaOrbWrite.addr := dmaOrbWriteAddr.field(Bits(32 bits), WO, doc = "Addr").asUInt
  io.dmaOrbWrite.len := dmaOrbWriteLen.field(Bits(32 bits), WO, doc = "Len").asUInt.resized

  bus.accept(HtmlGenerator("REG.html", "ORB SLAM"))
}

object Instruction extends App {
  SpinalVerilog(new Instruction(DMA_CONFIG(),DMA_CONFIG(),DMA_CONFIG()))
}
