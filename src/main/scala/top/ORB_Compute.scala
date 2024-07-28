package top
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
import config.Config._

object FAST_TYPE {
  val small = "auto"
  val full = "block"
  val block = "distributed"
}

case class ORB_ComputeConfig(fastType:String = FAST_TYPE.small, MEM_DEPTH : Int = 128, SIZE_WIDTH : Int = 11, TopSort:Int = -1, isBlock : Boolean = false, BSNum : Int = -1){
  val DATA_NUM : Int = 8
  val DATA_WIDTH : Int = 8
  val DATA_STREAM_WIDTH = DATA_NUM * DATA_WIDTH

  val resizeConfig = ResizeConfig1(MEM_DEPTH = MEM_DEPTH, SIZE_WIDTH = SIZE_WIDTH)
  val fastConfig = FastConfig(MEM_DEPTH = MEM_DEPTH,SIZE_WIDTH = SIZE_WIDTH, isBlock = isBlock)
  val rsBriefConfig = RSBriefConfig(MEM_DEPTH = MEM_DEPTH, SIZE_WIDTH = SIZE_WIDTH)
  val gaussianConfig = GaussianConfig(MEM_DEPTH = MEM_DEPTH, SIZE_WIDTH = SIZE_WIDTH)
  val blockConfig = FastBlockSuppressionConfig(MEM_DEPTH = 1024, SIZE_WIDTH = SIZE_WIDTH, BSNum = BSNum, isBlock = isBlock)
}

class ORB_Compute(config : ORB_ComputeConfig) extends Module{
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

    val done = out Bool();

    val debugInstruction = out Vec(Bits(32 bits), 9)
    val debugInstruction2 = out Vec(Bits(32 bits), 6)
    val debugClear = in Bool()
  }

  val sizeInCompute = new ImageSize(config.SIZE_WIDTH, ((io.sizeIn.colNum + 8)>>3).resize(config.SIZE_WIDTH) - 1, io.sizeIn.rowNum)

  val resize = new Resize(config.resizeConfig)
//  val fast:FastIO = config.fastType match {
//    case FAST_TYPE.small => new FastOrbSmall(config.fastConfig)
//    case FAST_TYPE.full => new FastOrbFull(config.fastConfig)
////    case block => "many"
//  }
  val fast = new FastOrbSmall(config.fastConfig);

  io.debugClear <> fast.io.debugClear

  val gaussian = new Gaussian(config.gaussianConfig)
  val blockSuppression = if(config.isBlock) new FastBlockSuppression(config.blockConfig) else null
  val rsBrief = new RSBriefOrb(config.rsBriefConfig)
  val fpDrop = new FpDrop(config.SIZE_WIDTH, config.DATA_WIDTH, 16, 16)
  val dataSum = new DataSum(config.SIZE_WIDTH, config.DATA_WIDTH, config.TopSort)
  //数据流
  val fifoR = StreamFifo(io.sData.payload, 16)
  val fifoF = StreamFifo(io.sData.payload, 16)
  val fifoG = StreamFifo(io.sData.payload, 16)
  fifoR.io.push.valid := io.sData.fire
  fifoF.io.push.valid := io.sData.fire
  fifoG.io.push.valid := io.sData.fire
  fifoR.io.push.payload := io.sData.payload
  fifoF.io.push.payload := io.sData.payload
  fifoG.io.push.payload := io.sData.payload
  io.sData.ready := fifoG.io.push.ready && fifoR.io.push.ready && fifoF.io.push.ready
  fifoG.io.pop <> gaussian.io.sData
  fifoF.io.pop <> fast.io.sData
  fifoR.io.pop <> resize.io.sData

  gaussian.io.mData <> rsBrief.io.sDataImage

  if(config.isBlock){
    blockSuppression.io.sData <> fast.io.mData
    blockSuppression.io.mData <> fpDrop.io.sData
    fpDrop.io.mData <> rsBrief.io.sDataFeaturePoint
  } else {
    fast.io.mData <> fpDrop.io.sData
    fpDrop.io.mData <> rsBrief.io.sDataFeaturePoint
  }


  resize.io.mData <> io.mDataImage

  dataSum.io.sData <> fpDrop.io.mData.toFlowFire
  dataSum.io.sDataRsBrief <> rsBrief.io.mDataRsBrief
  dataSum.io.done := fast.io.done
  io.done := fast.io.done
  if(config.isBlock) blockSuppression.io.done := fast.io.done

  dataSum.io.mdata <> io.mData
  dataSum.io.flush := io.start.rise(False)
  io.inputLength := dataSum.io.inputLength
  io.outputLength := dataSum.io.outputLength
  io.mDataLast := dataSum.io.mdataLast
  dataSum.io.topNum := io.topNum
  //控制流
  resize.io.start := io.start
  fast.io.start := io.start
  gaussian.io.start := io.start
  rsBrief.io.start := io.start
  if(config.isBlock) blockSuppression.io.start := io.start

  resize.io.mask := io.maskR
  gaussian.io.mask := io.maskG
  fast.io.mask := io.maskF

  resize.io.rowNumIn := sizeInCompute.rowNum
  resize.io.colNumIn := sizeInCompute.colNum.resized
  resize.io.colNumFlilterIn := io.colNumFlilterIn
  resize.io.colNumSrcIn := io.colNumSrcIn
  resize.io.rowNumSrcIn := io.rowNumSrcIn

  fast.io.sizeIn := sizeInCompute
  gaussian.io.colNumIn := sizeInCompute.colNum
  gaussian.io.rowNumIn := sizeInCompute.rowNum
  rsBrief.io.sizeIn := sizeInCompute
  fpDrop.io.sizeIn := io.sizeIn

  fast.io.threshold := io.threshold
  if(config.isBlock) fast.io.thresholdInit := io.thresholdInit
  gaussian.io.inValid := io.inValid

  addTouch(fast.io.sData)
  addTouch(fast.io.mData)

  val inputCnt = WaCounter(fast.io.mData.fire, 32, 640*480+100)
  val outputCnt = WaCounter(rsBrief.io.mDataRsBrief.fire, 32, 640*480+100)
  val outputFpCnt = WaCounter(fpDrop.io.mData.fire, 32, 640*480+100)

  when(io.debugClear.rise(False)){
    inputCnt.clear
    outputCnt.clear
    outputFpCnt.clear
  }

  io.debugInstruction(0) := RegNext(inputCnt.count.asBits)
  io.debugInstruction(1) := RegNext(outputCnt.count.asBits)
  io.debugInstruction(2) := RegNext(outputFpCnt.count.asBits)

  io.debugInstruction(3) := RegNext(fast.io.mData.valid.asBits(32 bits))
  io.debugInstruction(4) := RegNext(rsBrief.io.mDataRsBrief.valid.asBits(32 bits))
  io.debugInstruction(5) := RegNext(fpDrop.io.mData.valid.asBits(32 bits))

  io.debugInstruction(6) := RegNext(fast.io.mData.ready.asBits(32 bits))
  io.debugInstruction(7) := RegNext(rsBrief.io.mDataRsBrief.ready.asBits(32 bits))
  io.debugInstruction(8) := RegNext(fpDrop.io.mData.ready.asBits(32 bits))
  io.debugInstruction2 <> fast.io.debugInstruction
}

object ORB_Compute extends App {
  SpinalVerilog(new ORB_Compute(ORB_ComputeConfig(FAST_TYPE.small, 128,11,256,isBlock = true, BSNum = 3))).printPruned
}