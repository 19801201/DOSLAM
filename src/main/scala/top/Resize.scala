package top
import dataStructure.{FeaturePoint, FeaturePointOrb}
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import data.{WindowsConfig, _}
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}

case class ResizeConfig1(MEM_DEPTH : Int = 1024) {
  val DATA_WIDTH = 8//输入像素的位宽
  val DATA_SIZE = 8
  val DATA_STREAM_WIDTH = DATA_WIDTH * 8 //输入数据的总数
  val SIZE_WIDTH = 11  //输入大小的位宽
  val WINDOWS_SIZE = 2  //窗口的大小

  val windowsConfig = WindowsConfig(DATA_NUM = 10, WINDOWS_SIZE_H = 2, WINDOWS_SIZE_W = 1,MEM_DEPTH = MEM_DEPTH)
}

class Resize(config:ResizeConfig1 = ResizeConfig1()) extends Module{
  val io = new Bundle {
    val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
    val mData = master Stream Bits(config.DATA_STREAM_WIDTH bits)

    val start = in Bool()
    //从0开始计数
    //输入的行和列，大小
    val rowNumIn = in UInt (config.SIZE_WIDTH bits)//输入行数为rowNumIn+1
    val colNumIn = in UInt (config.SIZE_WIDTH - 3 bits)//输入列数为colNumIn + 1,每次包含八列数字，最后一列可能包含无效数据
    //经过位宽转换后的行和列
    //val rowNumFlilterIn = in UInt (config.SIZE_WIDTH bits)//输入行数为rowNumIn+1
    val colNumFlilterIn = in UInt (config.SIZE_WIDTH - 3 bits)//输入列数为colNumIn + 1,每次包含八列数字，最后一列可能包含无效数据
    //经过列无效筛选后的行和列
    val rowNumSrcIn = in UInt (config.SIZE_WIDTH bits) //窗口的输入大小。输入数据的个数不管到底有多少
    val colNumSrcIn = in UInt (config.SIZE_WIDTH bits) //输出数据的大小由上位机给出即可，完全不考虑边界问题。即输入即输出数据量

    val mask = in Bits(8 bits)
  }

  //1、位宽转换
  val conversion = new BitwidthConversion(BitwidthConversionConfig())
  //2、获得两行数据
  val windows2 = new syncWindows(config.windowsConfig)
  //3、进行1.25倍按比例缩放
  val resize = new ResizeLinear(ResizeConfig())

  conversion.io.start := io.start
  conversion.io.rowNumIn := io.rowNumIn
  conversion.io.colNumIn := io.colNumIn
//  conversion.io.colValidNumIn := io.colValidNumIn
  conversion.io.sData <> io.sData

  //只取前n列的数据
  val colCnt = WaCounter(conversion.io.mData.fire, config.SIZE_WIDTH - 3, io.colNumFlilterIn)
  val fliter = colCnt.count > io.colNumSrcIn


  windows2.io.start := io.start
  windows2.io.sData <> conversion.io.mData.throwWhen(fliter)
  windows2.io.sizeIn <> new ImageSize(SIZE_WIDTH = config.SIZE_WIDTH, colNum = io.colNumSrcIn, rowNum = io.rowNumSrcIn)

  resize.io.mask := io.mask
  resize.io.start := io.start
  resize.io.rowNumSrcIn := windows2.io.sizeOut.rowNum
  resize.io.colNumSrcIn := io.colNumSrcIn
  resize.io.sData.arbitrationFrom(windows2.io.mData)
  for(h <- 0 to 1){
    for(w <- 0 to 9){
      resize.io.sData.payload(h)(w) := windows2.io.mData.payload(h)(0)(w * 8 + 7 downto w * 8)
    }
  }
  resize.io.mData <> io.mData
}

object Resize extends App {
  SpinalVerilog(new Resize(ResizeConfig1())).printPruned
}
