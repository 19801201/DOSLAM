package top
import data.{ReflectionFillWindow, ReflectionFillWindowConfig, WindowsConfig, syncWindowsPadding}
import dataStructure.FeaturePoint
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import spinal.lib.experimental.chisel.Module
import utils.ImageSize

case class FastConfig(DATA_NUM : Int = 8,
                              MEM_DEPTH : Int = 1024,
                              SIZE_WIDTH : Int = 11
                             ) {
    val WINDOWS_H = 7
    val WINDOWS_W = 3
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    //创建的MEM个数
}

//class myOrbFastSync(config:FastConfig) extends Module{
//    val io = new Bundle { //给出输入得到输出结果
//        //增加运行速度，一次传输多个个数据
//        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
//        val mData = master Stream FeaturePoint(NMSConfig())
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val threshold = in UInt (config.DATA_WIDTH bits)
//        val size = slave(new ImageSize(config.SIZE_WIDTH))
//    }
//
//    val windows = new (ReflectionFillWindowConfig())
//    val fast = new OrbFast(FastConfig())
//
//    windows.io.colNumIn := io.colNumIn
//    windows.io.rowNumIn := io.rowNumIn
//    windows.io.start := io.start
//
//    windows.io.sData << io.sData
//    fast.io.sData << windows.io.mData
//    io.mData << fast.io.mData
//    windows.io.mReady := fast.io.sDataReady
//
//    fast.io.threshold := io.threshold
//    fast.io.colNumIn := io.colNumIn
//    fast.io.rowNumIn := io.rowNumIn
//    fast.io.start := io.start
//}

//class FastOrb(config:FastConfig) extends Module{
//    val io = new Bundle { //给出输入得到输出结果
//        //增加运行速度，一次传输多个个数据
//        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
//        val mData = master Stream FeaturePoint(NMSConfig())
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
//        val threshold = in UInt (config.DATA_WIDTH bits)
//    }
//    //1、生成窗口
//    val windows = new syncWindowsPadding(WindowsConfig(8, 7, 3, 128))
//    windows.io.start := io.start
//    windows.io.sData << io.sData
//    windows.io.sizeIn := io.sizeIn
//    //2、做fast特征检测
//
//
//}

class myOrbFast(config:FastConfig) extends Module{
    val io = new Bundle { //给出输入得到输出结果
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mData = master Stream FeaturePoint(NMSConfig())
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val threshold = in UInt (config.DATA_WIDTH bits)
    }

    val windows = new ReflectionFillWindow(ReflectionFillWindowConfig())
    val fast = new OrbFast(FastConfig())

    windows.io.colNumIn := io.colNumIn
    windows.io.rowNumIn := io.rowNumIn
    windows.io.start := io.start

    windows.io.sData << io.sData
    fast.io.sData << windows.io.mData
    io.mData << fast.io.mData
    windows.io.mReady := fast.io.sDataReady

    fast.io.threshold := io.threshold
    fast.io.colNumIn := io.colNumIn
    fast.io.rowNumIn := io.rowNumIn
    fast.io.start := io.start
}

class OrbFast(config:FastConfig) extends Module{
    val io = new Bundle {//给出输入得到输出结果
        //增加运行速度，一次传输多个个数据
        val sData = slave Flow Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_H)
        val sDataReady = out Bool()
        val mData = master Stream FeaturePoint(NMSConfig())
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val threshold = in UInt(config.DATA_WIDTH bits)
    }
    //状态机处理
    val fsm = new StateMachine {
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val END = new State
        //两种情况下会输出有效数据，第一种接收到有效数据，第二种mready为真，输出有效数据

        val colCnt = WaCounter(isActive(VALID) && io.sData.fire, io.colNumIn.getWidth, io.colNumIn - 1)
        val rowCnt = WaCounter(isActive(VALID) && io.sData.fire && colCnt.valid, io.rowNumIn.getWidth, io.rowNumIn - 1)

        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive { //行列都为0
              when(io.sData.fire && colCnt.valid && rowCnt.valid) {
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期
              goto(IDLE)
          }
    }
    //1、整理数据
    def windowsReg(rdData: Vec[Bits], wen: Bool): Vec[Vec[Bits]] = {
        val windows = Vec(Vec(Reg(Bits(config.DATA_STREAM_WIDTH bits)) init 0, config.WINDOWS_W), config.WINDOWS_H)
        for (h <- 0 until config.WINDOWS_H) {
            for (w <- 0 until config.WINDOWS_W) {
                if (w == config.WINDOWS_W - 1) {
                    when(wen) {
                        windows(h)(w) := rdData(h)
                    }
                } else {
                    when(wen) {
                        windows(h)(w) := windows(h)(w + 1)
                    }
                }
            }
        }
        windows //得到若干行的结果
    }

    def formatCornerScoreWindos(windows: Vec[Vec[Bits]]): Vec[Vec[Bits]] = {
        val temp = Vec(Vec(Bits(config.DATA_WIDTH bits), cornerScoreConfig().DATA_NUM), config.DATA_NUM)
        for (i <- 0 until config.DATA_NUM) {
            temp(i)(0) := windows(3)(1).subdivideIn(config.DATA_NUM slices)(i)
            for (j <- 1 until cornerScoreConfig().DATA_NUM) {
                temp(i)(j) := windows(FastDetectionConfig().y(j - 1) + 3).asBits.subdivideIn(config.WINDOWS_W * config.DATA_NUM slices)(FastDetectionConfig().x(j - 1) + 8 + i)
                //temp(i)(j) := (windows(FastDetectionConfig().y(j - 1) + 3)(0) ## windows(FastDetectionConfig().y(j - 1) + 3)(1) ## windows(FastDetectionConfig().y(j - 1) + 3)(2)).subdivideIn(config.WINDOWS_W * config.DATA_NUM slices)(FastDetectionConfig().x(j - 1) + 8 + i)
            }
        }
        temp
    }
    //2、给FASTDetection 将结果传入
    //3、给cornerScore

    val compute = new Area{//计算模块，给出特征点和有效信号
        val delayValid1 = Delay(((fsm.isActive(fsm.VALID) && io.sData.fire && (fsm.colCnt.count.orR || fsm.rowCnt.count.orR))|| fsm.isActive(fsm.END)), 1)
        val inValidRow = Delay(fsm.rowCnt.count > 2 && (fsm.rowCnt.count + 3) < io.rowNumIn, 2)
        val inValidCol1 = Delay(fsm.colCnt.count.orR, 2)
        val inValidCol2 = Delay(fsm.colCnt.valid, 2)

        val windows = windowsReg(io.sData.payload, io.sData.fire)//生成所需要的窗口数据

        val fastDetectionWindows = formatCornerScoreWindos(windows)
        val cornerWindows = Delay(fastDetectionWindows, 1)
        val cornerData = Vec(Bits(config.DATA_WIDTH bits), config.DATA_NUM)
        val cornerValid = Vec(Bool(), config.DATA_NUM)
        for(i <- 0 until config.DATA_NUM){
            val fastDetection = new FastDetection(FastDetectionConfig())
            fastDetection.io.sData.payload := fastDetectionWindows(i)
            if(i < 3){
                fastDetection.io.sData.valid := delayValid1 && inValidRow && inValidCol1//这个逻辑判断条件应该有问题
            } else if(i > 4){
                fastDetection.io.sData.valid := delayValid1 && inValidRow && !inValidCol2
            } else {
                fastDetection.io.sData.valid := delayValid1 && inValidRow
            }

            fastDetection.io.threshold := io.threshold
            val corner = new CornerScore(cornerScoreConfig())
            corner.io.sData.payload := cornerWindows(i)
            corner.io.sData.valid := fastDetection.io.mData.valid && (fastDetection.io.mData.payload(0) || fastDetection.io.mData.payload(1))
            corner.io.islight := fastDetection.io.mData.payload(0)
            corner.io.mData.payload <> cornerData(i)
            corner.io.mData.valid <> cornerValid(i)
        }
    }
    //4、传入FIFO
    val dataFifo = new Area{
        val cornerDataSel = Vec(Bits(config.DATA_WIDTH bits), config.DATA_NUM)
        for(i <- 0 until config.DATA_NUM){
            when(compute.cornerValid(i)){
                cornerDataSel(i) := compute.cornerData(i)
            } otherwise{
                cornerDataSel(i) := 0
            }
        }
        val fifo = StreamFifo(cornerDataSel.asBits, 128)
        fifo.io.push.valid := Delay(compute.delayValid1, 5)
        fifo.io.push.payload := cornerDataSel.asBits

        //io.sReady.setAsReg() init(False)
        when(fifo.io.availability > 10){
            io.sDataReady := True
        } otherwise{
            io.sDataReady := False
        }
    }
    //5、给出结果
    val dataNms = new Area{
        val nms = new NMS(NMSConfig())
        nms.io.sData <> dataFifo.fifo.io.pop
        nms.io.colNumIn <> io.colNumIn
        nms.io.rowNumIn <> io.rowNumIn
        nms.io.start <> io.start
        nms.io.mData <> io.mData
    }
}

class Fast(config:FastConfig) extends Module{
    val io = new Bundle {//给出输入得到输出结果
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_H)
        val mData = master Stream FeaturePoint(NMSConfig())
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val threshold = in UInt(config.DATA_WIDTH bits)
    }
    //状态机处理
    val fsm = new StateMachine {
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val END = new State
        //两种情况下会输出有效数据，第一种接收到有效数据，第二种mready为真，输出有效数据

        val colCnt = WaCounter(isActive(VALID) && io.sData.fire, io.colNumIn.getWidth, io.colNumIn - 1)
        val rowCnt = WaCounter(isActive(VALID) && io.sData.fire && colCnt.valid, io.rowNumIn.getWidth, io.rowNumIn - 1)

        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive { //行列都为0
              when(io.sData.fire && colCnt.valid && rowCnt.valid) {
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期
              goto(IDLE)
          }
    }
    //1、整理数据
    def windowsReg(rdData: Vec[Bits], wen: Bool): Vec[Vec[Bits]] = {
        val windows = Vec(Vec(Reg(Bits(config.DATA_STREAM_WIDTH bits)) init 0, config.WINDOWS_W), config.WINDOWS_H)
        for (h <- 0 until config.WINDOWS_H) {
            for (w <- 0 until config.WINDOWS_W) {
                if (w == config.WINDOWS_W - 1) {
                    when(wen) {
                        windows(h)(w) := rdData(h)
                    }
                } else {
                    when(wen) {
                        windows(h)(w) := windows(h)(w + 1)
                    }
                }
            }
        }
        windows //得到若干行的结果
    }

    def formatCornerScoreWindos(windows: Vec[Vec[Bits]]): Vec[Vec[Bits]] = {
        val temp = Vec(Vec(Bits(config.DATA_WIDTH bits), cornerScoreConfig().DATA_NUM), config.DATA_NUM)
        for (i <- 0 until config.DATA_NUM) {
            temp(i)(0) := windows(3)(1).subdivideIn(config.DATA_NUM slices)(i)
            for (j <- 1 until cornerScoreConfig().DATA_NUM) {
                temp(i)(j) := windows(FastDetectionConfig().y(j - 1) + 3).asBits.subdivideIn(config.WINDOWS_W * config.DATA_NUM slices)(FastDetectionConfig().x(j - 1) + 8 + i)
                //temp(i)(j) := (windows(FastDetectionConfig().y(j - 1) + 3)(0) ## windows(FastDetectionConfig().y(j - 1) + 3)(1) ## windows(FastDetectionConfig().y(j - 1) + 3)(2)).subdivideIn(config.WINDOWS_W * config.DATA_NUM slices)(FastDetectionConfig().x(j - 1) + 8 + i)
            }
        }
        temp
    }
    //2、给FASTDetection 将结果传入
    //3、给cornerScore

    val compute = new Area{//计算模块，给出特征点和有效信号
        val delayValid1 = Delay(((fsm.isActive(fsm.VALID) && io.sData.fire && (fsm.colCnt.count.orR || fsm.rowCnt.count.orR))|| fsm.isActive(fsm.END)), 1)
        val inValidRow = Delay(fsm.rowCnt.count > 2 && (fsm.rowCnt.count + 3) < io.rowNumIn, 2)
        val inValidCol1 = Delay(fsm.colCnt.count.orR, 2)
        val inValidCol2 = Delay(fsm.colCnt.valid, 2)

        val windows = windowsReg(io.sData.payload, io.sData.fire)//生成所需要的窗口数据

        val fastDetectionWindows = formatCornerScoreWindos(windows)
        val cornerWindows = Delay(fastDetectionWindows, 1)
        val cornerData = Vec(Bits(config.DATA_WIDTH bits), config.DATA_NUM)
        val cornerValid = Vec(Bool(), config.DATA_NUM)
        for(i <- 0 until config.DATA_NUM){
            val fastDetection = new FastDetection(FastDetectionConfig())
            fastDetection.io.sData.payload := fastDetectionWindows(i)
            if(i < 3){
                fastDetection.io.sData.valid := delayValid1 && inValidRow && inValidCol1//这个逻辑判断条件应该有问题
            } else if(i > 4){
                fastDetection.io.sData.valid := delayValid1 && inValidRow && !inValidCol2
            } else {
                fastDetection.io.sData.valid := delayValid1 && inValidRow
            }

            fastDetection.io.threshold := io.threshold
            val corner = new CornerScore(cornerScoreConfig())
            corner.io.sData.payload := cornerWindows(i)
            corner.io.sData.valid := fastDetection.io.mData.valid && (fastDetection.io.mData.payload(0) || fastDetection.io.mData.payload(1))
            corner.io.islight := fastDetection.io.mData.payload(0)
            corner.io.mData.payload <> cornerData(i)
            corner.io.mData.valid <> cornerValid(i)
        }
    }
    //4、传入FIFO
    val dataFifo = new Area{
        val cornerDataSel = Vec(Bits(config.DATA_WIDTH bits), config.DATA_NUM)
        for(i <- 0 until config.DATA_NUM){
            when(compute.cornerValid(i)){
                cornerDataSel(i) := compute.cornerData(i)
            } otherwise{
                cornerDataSel(i) := 0
            }
        }
        val fifo = StreamFifo(cornerDataSel.asBits, 128)
        fifo.io.push.valid := Delay(compute.delayValid1, 5)
        fifo.io.push.payload := cornerDataSel.asBits

        //io.sReady.setAsReg() init(False)
        when(fifo.io.availability > 10){
            io.sData.ready := True
        } otherwise{
            io.sData.ready := False
        }
    }
    //5、给出结果
    val dataNms = new Area{
        val nms = new NMS(NMSConfig())
        nms.io.sData <> dataFifo.fifo.io.pop
        nms.io.colNumIn <> io.colNumIn
        nms.io.rowNumIn <> io.rowNumIn
        nms.io.start <> io.start
        nms.io.mData <> io.mData
    }
}

object Fast extends App {
    SpinalVerilog(new Fast(FastConfig()))
}