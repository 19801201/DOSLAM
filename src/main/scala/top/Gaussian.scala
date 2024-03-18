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
case class GaussianConfig(DATA_NUM : Int = 8,
                              MEM_DEPTH : Int = 1024,
                              SIZE_WIDTH : Int = 11
                             ) {
  val WINDOWS_SIZE_H = 7
  val DATA_WIDTH = 8
  val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
  //创建的MEM个数
}

class Gaussian(config:GaussianConfig) extends Module{
  val io = new Bundle {
    val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
    val mData = master Stream Bits(config.DATA_STREAM_WIDTH bits)
    //输入信号和输出信号，确保size*size个数据同时输出
    val start = in Bool()
    //开始信号
    //图像尺寸，从0开始
    val rowNumIn = in UInt (config.SIZE_WIDTH bits)
    val colNumIn = in UInt (config.SIZE_WIDTH bits)
    val inValid = in Bits (3 bits)
  }

  val rfWindows = new ReflectionFillWindow(ReflectionFillWindowConfig())
  val compute = new GaussianBlur(GaussianBlurConfig())
  val fifo = new StreamFifo(compute.io.mData.payload, 64)

  rfWindows.io.start := io.start
  compute.io.start := io.start
  rfWindows.io.colNumIn := io.colNumIn
  rfWindows.io.rowNumIn := io.rowNumIn
  compute.io.colNumIn := io.colNumIn
  compute.io.rowNumIn := io.rowNumIn
  compute.io.inValid := io.inValid

  rfWindows.io.mReady := RegNext(fifo.io.availability > 50, True)
  //数据流
  io.sData <> rfWindows.io.sData
  rfWindows.io.mData <> compute.io.sData
  fifo.io.push.payload := compute.io.mData.payload
  fifo.io.push.valid := compute.io.mData.valid
  fifo.io.pop <> io.mData
}


object Gaussian extends App {
  SpinalVerilog(new Gaussian(GaussianConfig())).printPruned
}
