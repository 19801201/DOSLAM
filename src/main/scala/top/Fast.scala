package top
import data.{ReflectionFillWindow, ReflectionFillWindowConfig, WindowsConfig, formatCornerScoreWindos, syncWindowsPadding, syncWindowsPadding2}
import dataStructure.{FeaturePoint, FeaturePointOrb}
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}

case class FastConfig(DATA_NUM : Int = 8,
                              MEM_DEPTH : Int = 1024,
                              SIZE_WIDTH : Int = 11
                             ) {
    val WINDOWS_H = 7
    val WINDOWS_W = 3
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    //创建的MEM个数
    val MASK4 = Array(
        M"-------1",
        M"------10",
        M"-----100",
        M"----1000",
        M"---10000",
        M"--100000",
        M"-1000000",
        M"10000000")
}


class FastIO(config:FastConfig) extends Module {
    val io = new Bundle { //给出输入得到输出结果
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mData = master Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH)
        //        val mData = master Stream Bits(config.DATA_STREAM_WIDTH bits)
        //        val temp = master Flow(Vec(Vec(Bits(config.DATA_WIDTH bits), cornerScoreConfig().DATA_NUM), config.DATA_NUM))
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
        val threshold = in UInt (config.DATA_WIDTH bits)
        val mask = in Bits(16 bits)
    }
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
//这是每次处理8像素点每个像素都进行全并行计算，8个特征检测模块，8个得分点计算模块
class FastOrbFull(config:FastConfig) extends FastIO(config){
    //1、生成窗口
    val windows = new syncWindowsPadding2(WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 7, WINDOWS_SIZE_W = 3,
        MEM_DEPTH = 128))
    windows.io.start := io.start
    windows.io.sData <> io.sData
    windows.io.sizeIn := io.sizeIn
    //2、做fast特征检测
    val fastDetectionWindows = formatCornerScoreWindos(windows.io.mData.payload, config)
//    io.temp.payload := fastDetectionWindows
//    io.temp.valid := windows.io.mData.fire
    val fastDetectionWindows_r = Delay(fastDetectionWindows, 1)
    val isFastFeaturePoints, isFastFeatureLight = Bits(config.DATA_NUM bits)
    val cornerValid = Bits(config.DATA_NUM bits)
    val cornerData  = Vec(Bits(config.DATA_WIDTH bits), config.DATA_NUM)

    val validCnt = ImageCount(windows.io.mData.fire, io.sizeIn)
    val rowValid = RegNextWhen(validCnt.rowValid(U(3, 2 bits), io.sizeIn.rowNum - 3), windows.io.mData.fire, False)

    val debugColValid = Bits(config.DATA_NUM bits)
    fastDetectionWindows.zipWithIndex.foreach((d) => {
        val (data, i) = d
        val colValidSize = validCnt.getSize().setSel(U(i, 3 bits))
        val colValid = RegNextWhen(colValidSize.colNum >= 3 && colValidSize.colNum <= ((io.sizeIn.colNum << 3) + 4), windows.io.mData.fire, False)
        //做特征检测
        debugColValid(i) := colValid
        val fastDetection = new FastDetection(FastDetectionConfig())
        fastDetection.io.sData.payload := data
        fastDetection.io.sData.valid := windows.io.mData.fire
        fastDetection.io.threshold := io.threshold
        isFastFeaturePoints(i) := fastDetection.io.mData.fire && fastDetection.io.mData.payload.orR && colValid && rowValid
        isFastFeatureLight(i) := fastDetection.io.mData.payload(0)
        //3、计算分数
        val corner = new CornerScore(cornerScoreConfig())
        corner.io.sData.payload := fastDetectionWindows_r(i)
        corner.io.sData.valid := isFastFeaturePoints(i)
        corner.io.islight := fastDetection.io.mData.payload(0)
        cornerData(i) := corner.io.mData.valid.mux(corner.io.mData.payload, B(0, config.DATA_WIDTH bits))
    })
    //得到的结果存入fifo
    val fifo = new StreamFifo(Bits(config.DATA_STREAM_WIDTH bits), 128)
    fifo.io.push.valid := Delay(windows.io.mData.fire, 5)

    val validMaskCnt = WaCounter(fifo.io.push.fire, config.SIZE_WIDTH, io.sizeIn.colNum)
    val validMaskValidS = validMaskCnt.count === RegNext(io.sizeIn.colNum - 1)
    for(i <- 0 until config.DATA_NUM){
        fifo.io.push.payload.subdivideIn(config.DATA_NUM slices)(i) := (validMaskCnt.validLast() && io.mask(i) || validMaskValidS && io.mask(i + 8)).mux(B(0, config.DATA_WIDTH bits), cornerData(i))
    }
    //io.sReady.setAsReg() init(False)
    when(fifo.io.availability > 10) {
        windows.io.mData.ready := True
    } otherwise {
        windows.io.mData.ready := False
    }
    //得到的结果传入NMS
    val nms = new NMS1(NMSConfig())
    nms.io.sData <> fifo.io.pop
    nms.io.start <> io.start
    nms.io.sizeIn := windows.io.sizeIn
    nms.io.mData <> io.mData
//
//    io.mData <> fifo.io.pop
}
//这里非全并行模块，根据特征点生成的概率，8个特征检测模块，配备一个得分点比较模块
class FastOrbSmall(config:FastConfig) extends FastIO(config) {
//    val io = new Bundle { //给出输入得到输出结果
//        //增加运行速度，一次传输多个个数据
//        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
//        val mData = master Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH)
////        val mData = master Stream Bits(config.DATA_STREAM_WIDTH bits)
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
//        val threshold = in UInt (config.DATA_WIDTH bits)
//        val mask = in Bits(16 bits)
//    }
    //1、生成窗口
    val fifoReady = Bool()
    val windows = new syncWindowsPadding2(WindowsConfig(8, 7, 3, 128))
    windows.io.start := io.start
//    windows.io.sData.payload := io.sData.payload
//    windows.io.sData.valid := io.sData.valid && fifoReady
//    io.sData.ready := windows.io.sData.ready && fifoReady
    windows.io.sData <> io.sData
    windows.io.sizeIn := io.sizeIn
    //2、做fast特征检测 得分点计算
    val fastDetectionWindows = windows.io.mData.continueWhen(fifoReady).translateWith(formatCornerScoreWindos(windows.io.mData.payload, config))
    val fastDetectionWindows_r = fastDetectionWindows.m2sPipe(holdPayload = true)
    val isFastFeatureLight, isFastFeaturePoints = Bits(config.DATA_NUM bits)
    val isNextFastFeaturePoints,isNextFastFeatureLight = Reg(Bits(config.DATA_NUM bits))
    val curMaxPointValidLess1 = Bool()
    //有效数据计算
    val validCnt = ImageCount(windows.io.mData.fire, io.sizeIn)
    val rowValid = RegNextWhen(validCnt.rowValid(U(3, 2 bits), io.sizeIn.rowNum - 3), windows.io.mData.fire, False)

    fastDetectionWindows.payload.zipWithIndex.foreach((d) =>{
        val (data, i) = d

        val colValidSize = validCnt.getSize().setSel(U(i, 3 bits))
        val colValid = RegNextWhen(colValidSize.colNum >= 3 && colValidSize.colNum <= ((io.sizeIn.colNum << 3) + 4), windows.io.mData.fire, False)

        val fastDetection = new FastDetection(FastDetectionConfig())
        fastDetection.io.sData.payload := data
        fastDetection.io.sData.valid := fastDetectionWindows.fire
        fastDetection.io.threshold := io.threshold
        isFastFeaturePoints(i) := fastDetection.io.mData.fire && fastDetection.io.mData.payload.orR && colValid && rowValid
        isFastFeatureLight(i) := fastDetection.io.mData.payload(0)
        when(fastDetection.io.mData.fire){
            isNextFastFeaturePoints(i) := isFastFeaturePoints(i)
            isNextFastFeatureLight(i) := isFastFeatureLight(i)
        }
    })
    //3、计算分数
    //fastDetectionWindows_r是计算所需要的数据，
    //CornerScore延迟4个周期

    val mask = Reg(Bits(config.DATA_NUM bits)) init (0)
    val FastFeaturePointsNeed = Delay(fastDetectionWindows.fire, 1).mux(isFastFeaturePoints, isNextFastFeaturePoints)
    val FastFeatureLightNeed = Delay(fastDetectionWindows.fire, 1).mux(isFastFeatureLight, isNextFastFeatureLight)
    val curMaxPointValid = mask ^ FastFeaturePointsNeed
    val key = OHToUInt(Vec(config.MASK4.map(mask => mask === curMaxPointValid))) //选择出有效数据 得到key
    when(windows.io.mData.fire) {
        mask := B(0, config.DATA_NUM bits)
    } elsewhen (curMaxPointValid.orR) {
        mask := mask | UIntToOh(key)
    }
    val score = new CornerScore(cornerScoreConfig())
    score.io.sData.payload := fastDetectionWindows_r.payload.read(key)
    score.io.sData.valid := fastDetectionWindows_r.valid && curMaxPointValid.orR
    score.io.islight := FastFeatureLightNeed(key)
    fastDetectionWindows_r.ready := curMaxPointValidLess1
    curMaxPointValidLess1 := (curMaxPointValid.subdivideIn(config.DATA_NUM slices).map(p => p.asUInt).reduceBalancedTree(_ +^ _) <= U"3'b1")
    //4、结果保存
    val saveKey = Delay(key, 4)
    val saveCurMaxPointValid = Delay(curMaxPointValid, 4)
    val fifo = StreamFifo(Bits(config.DATA_STREAM_WIDTH bits), 64)
    fifoReady := RegNext(fifo.io.availability >= 8)
    val saveData = Vec(Reg(Bits(config.DATA_WIDTH bits)) init 0, config.DATA_NUM)
    when(Delay(fastDetectionWindows_r.fire, 5)) {
        saveData.foreach(_ := B(0, config.DATA_WIDTH bits))
    }
    when(score.io.mData.fire){
        saveData.write(saveKey, score.io.mData.payload)
    }
    val validMaskCnt = WaCounter(fifo.io.push.fire, config.SIZE_WIDTH, io.sizeIn.colNum)
    val validMaskValidS = validMaskCnt.count === RegNext(io.sizeIn.colNum - 1)
    for(i <- 0 until config.DATA_NUM){
        fifo.io.push.payload.subdivideIn(config.DATA_NUM slices)(i) := (validMaskCnt.validLast() && io.mask(i) || validMaskValidS && io.mask(i + 8)).mux(B(0, config.DATA_WIDTH bits), saveData(i))
    }

    fifo.io.push.valid := Delay((saveCurMaxPointValid.subdivideIn(config.DATA_NUM slices).map(p => p.asUInt).reduceBalancedTree(_ +^ _) <= U"3'b1") && Delay(fastDetectionWindows_r.valid, 4), 1)
    //将结果传给NMS
    val nms = new NMS1(NMSConfig())
    nms.io.sData <> fifo.io.pop
    nms.io.start <> io.start
    nms.io.sizeIn := windows.io.sizeOut
    nms.io.mData <> io.mData
//    io.mData <> fifo.io.pop
}

//class myOrbFast(config:FastConfig) extends Module{
//    val io = new Bundle { //给出输入得到输出结果
//        //增加运行速度，一次传输多个个数据
//        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
//        val mData = master Stream FeaturePoint(NMSConfig())
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
//        val colNumIn = in UInt (config.SIZE_WIDTH bits)
//        val threshold = in UInt (config.DATA_WIDTH bits)
//    }
//
//    val windows = new ReflectionFillWindow(ReflectionFillWindowConfig())
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
//原始的fast特征检测，NMS反压应该存在错误，替换位NMS1应该可以正常使用，使用老版的特征检测，暂时不用了
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

object FastOrbFull extends App {
    SpinalVerilog(new FastOrbFull(FastConfig()))
}

object FastOrb extends App {
    SpinalVerilog(new FastOrbSmall(FastConfig()))
}