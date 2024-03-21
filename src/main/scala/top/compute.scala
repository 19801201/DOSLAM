package top

import data._
import dataStructure.FeaturePoint
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import spinal.lib.experimental.chisel.Module

case class computeConfig(DATA_NUM : Int = 8,
                      MEM_DEPTH : Int = 1024,
                      SIZE_WIDTH : Int = 11
                     ) {
    val WINDOWS_H = 7
    val WINDOWS_W = 3
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM

    val dataGenerateRow7Config = ReflectionFillWindowConfig(DATA_NUM = 8, MEM_DEPTH = 1024, SIZE_WIDTH = SIZE_WIDTH)
}

class compute(config:computeConfig) extends Module {
    val io = new Bundle { //给出输入得到输出结果
        //图像的输入，一次输入8个像素，因此图像的宽度必须为8的倍数
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mResizeData = master Stream Bits(config.DATA_STREAM_WIDTH bits)
        //val mData = master Stream FeaturePoint(NMSConfig())
        //开始信号
        val start = in Bool()
        //输入图像的大小
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val colValidNumIn = in UInt (3 bits)
        //位宽变化之后的图像大小
        val rowNumIn1 = in UInt (config.SIZE_WIDTH bits)
        val colNumIn2 = in UInt (config.SIZE_WIDTH bits)
        val colValidNumIn1 = in UInt (3 bits)
        //最后一列的有效数据个数为i+1
        //输入阈值的大小
        val threshold = in UInt (config.DATA_WIDTH bits)
    }
    //给下层信号的valid，需要两个ready都拉高才可以。
    val fastReady, gaussianReady, resizeReady = Bool()

    def dstributeValid(inValid : Bool, aReady : Bool, bReady : Bool): Bool = { //这个值有效才可以真正传递给后续数据
        inValid & aReady & bReady
    }
    //1、对输入数据进行resize计算并给出
    def resize(dataIn : Flow[Vec[Bits]]): Unit = {//数据转换
        //1、位宽转换
        val conversion1 = new BitwidthConversion(BitwidthConversionConfig())
        val conversion2 = new BitwidthConversion(BitwidthConversionConfig())
        //2、进行1.25倍按比例缩放
        val resize = new ResizeLinear(ResizeConfig())

        resizeReady <> (conversion1.io.sData.ready && conversion2.io.sData.ready)

    }

    def fast(dataIn : Flow[Vec[Bits]]): Unit = {//进行FAST模块的计算
        val fast = new Fast(FastConfig())
        fast.io.sData.payload <> dataIn.payload
        fast.io.sData.valid <> dataIn.valid
        fastReady <> fast.io.sData.ready
    }

    def gaussianBlur(dataIn : Flow[Vec[Bits]]): Unit = {//进行高斯模糊计算

    }

    def dataGenerateRow7(): Flow[Vec[Bits]] = {
        val windows = new ReflectionFillWindow(config.dataGenerateRow7Config)
        windows.io.start <> io.start
        windows.io.sData <> io.sData
        windows.io.rowNumIn <> io.rowNumIn
        windows.io.colNumIn <> io.colNumIn
        windows.io.mReady <> (fastReady & gaussianReady & resizeReady)

        windows.io.mData
        //这个信号如果拉低，下级模块
    }
}