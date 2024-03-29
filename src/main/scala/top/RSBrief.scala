package top
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import data._
import dataStructure.{FeaturePoint, FeaturePointOrb}
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}
case class RSBriefConfig(DATA_NUM : Int = 8,
                      MEM_DEPTH : Int = 128,
                      SIZE_WIDTH : Int = 11
                     ) {
    val WINDOWS_H = 7
    val WINDOWS_W = 3
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    //创建的MEM个数
    val dataGenerateRow31Config = WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 31, WINDOWS_SIZE_W = 5, MEM_DEPTH = MEM_DEPTH, SIZE_WIDTH = SIZE_WIDTH,useFlip = false)
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

class RSBrief(config:RSBriefConfig) extends Module {
    val io = new Bundle {
        //输入信号和输出信号，确保size*size个数据同时输出
        val sDataImage = slave Stream Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_H)
        val sDataFeaturePoint = slave Stream FeaturePoint(NMSConfig())

        val mDataRsBrief = master Stream Bits(64 bits)
        val start = in Bool()

        //图片尺寸大小
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val inValid = in Bits (3 bits)
    }
/*
    生成31行 * 5列大小窗口的数据
 */
    def dataGenerate(inputFire : Bool, sReady : Bool): Stream[Vec[Vec[Bits]]] = {
        //1、设置高斯模糊 FLOW类型，
        val gaussianBlur = new GaussianBlur(GaussianBlurConfig())
        io.sDataImage.payload <> gaussianBlur.io.sData.payload
        io.sDataImage.fire <> gaussianBlur.io.sData.valid
        io.start <> gaussianBlur.io.start
        io.colNumIn <> gaussianBlur.io.colNumIn
        io.rowNumIn <> gaussianBlur.io.rowNumIn
        io.inValid <> gaussianBlur.io.inValid
        //2、存储数据进行缓冲
        val fifo = StreamFifo(Bits(config.DATA_STREAM_WIDTH bits), config.MEM_DEPTH)
        fifo.io.push.payload <> gaussianBlur.io.mData.payload
        fifo.io.push.valid <> gaussianBlur.io.mData.valid
        io.sDataImage.ready := fifo.io.availability >= 128 && sReady
        inputFire := fifo.io.pop.fire
        //3、得到的结果进入windows窗口，产生31行，5列的数据,windwos模块的ready应该怎么控制？
        val windows31 = new syncWindows(config.dataGenerateRow31Config)
        windows31.io.sData <> fifo.io.pop
        windows31.io.start <> io.start
        windows31.io.sizeIn.colNum <> io.colNumIn
        windows31.io.sizeIn.rowNum <> io.rowNumIn

        windows31.io.mData
    }
    //处理特征点的输入，如果没有数据那么就让特征点进入模块内部 需要给内部的ready信号 使用两个valid信号
    //featurePoint 1 是主要用到的，0是当1计算过程的时候 1用于消除windows的气泡
    def inputFeaturePoint(output : Vec[Stream[FeaturePoint]]): Unit = {

        output(0) <> io.sDataFeaturePoint
        output(0).ready := (output(1).valid | output(1).ready)
        output(1).valid.setAsReg() init False
        output(1).payload.setAsReg()
        when(output(0).fire){
            output(1).valid := True
            output(1).payload := output(0).payload
        } elsewhen(output(1).fire){
            output(1).valid := False
        }
    }
    //给定窗口数据，从中选择出需要计算得数据 延迟一个周期 根据sel选择，当valid有效时
    def selData(input : Flow[Vec[Vec[UInt]]], sel : Bits, end : Bool): Flow[Vec[Vec[UInt]]] = {
        val selData0 = Vec(Vec(UInt(8 bits), 31), 31)
        val selData1 = Vec(Vec(Reg(UInt(8 bits), U(0)), 31), 31)
        for (j <- 0 to 30) { //选择不同情况的数据进行传输
            switch(sel) {
                for (i <- 0 to 7) {
                    is(i) { //偏移i个值
                        for (k <- 0 to 30) {
                            selData0(j)(k) := input.payload(j).asBits.subdivideIn(5 * 8 slices)(k + i + 1).asUInt
                        }
                    }
                }
            }
        }
        when(input.valid){
            selData1 := selData0
        }
        val ret = new Flow(selData1)
        val dataHave = Reg(Bool()) init False
        val dataCount = Reg(UInt(3 bits)) init (0)
        when(input.valid){//数据来临之后拉高8个周期，
            dataHave := True
        } elsewhen (dataCount.andR){
            dataHave := False
        }
        end := dataCount.andR
        when(dataHave){
            dataCount := dataCount + 1
        }
        ret.valid := dataHave
        ret.payload := selData1
        ret
    }
    //当inputValid拉高时，保证inputData数据有效且保存8个周期，让brief核icAngle等模块计算出最后得描述子得结果
    def compute(input : Flow[Vec[Vec[UInt]]], sReady : Bool) : Unit = {
        val brief = new BRIEF(BRIEFConfig()) //1
        val icAngle = new IC_Angle(IC_AngleConfig()) //15
        val tan = new tan2(tan2Config())
        val rotate = new Rotate(rotateConfig())
        brief.io.sData.payload <> input.payload
        brief.io.sData.valid <> input.valid
        icAngle.io.sData.payload <> input.payload
        icAngle.io.sData.valid <> input.valid
        tan.io.sData <> icAngle.io.mData
        rotate.io.sDataTan.payload <> tan.io.mData.payload
        rotate.io.sDataTan.valid <> tan.io.mData.valid //这个ready信号控制 忽略即可
        sReady := rotate.io.sReady
        rotate.io.sDataBrief.payload <> brief.io.mData.payload.asBits
        rotate.io.sDataBrief.valid <> brief.io.mData.valid
        rotate.io.mDataRsBrief <> io.mDataRsBrief
    }

    //3、状态机记录记录给windosw31窗口数据的位置，
    val fsm = new StateMachine {
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val WAIT = new State
        val VALID = new State
        val END = new State
        //一共有三个状态IDLE准备状态，接受到start信号上升沿开始工作 进入valid状态
        //VALID 工作状态，只有valid状态才正常工作 当接受完所有数据，并且所有数据都已经输出那么进入END状态
        //END状态，
        val dataFire = Bool()
        //在这里进行输入控制。a::一定是每来一个数据才曾加1
        //一次输入八个数据这样可以更快的计算，
        val colCnt = WaCounter(dataFire, config.SIZE_WIDTH, io.colNumIn - 1)
        val rowCnt = WaCounter(dataFire && colCnt.valid, config.SIZE_WIDTH, io.rowNumIn - 1)

        //窗口数据所在位置
        val featurePointColCnt = WaCounter(dataFire && (colCnt.count >= U(4)), config.SIZE_WIDTH, io.colNumIn - 3, 2)
        //前16个点的数据都忽略 起始位置就是2,什么时候变动？pop给出数据之后，并且windows可以形成一个窗口的数据,如果已经存储了4个数据，那么下一个数据到达就可以形成一个窗口
        val featurePointRowCnt = WaCounter(dataFire && featurePointColCnt.valid && (rowCnt.count >= U(30)), config.SIZE_WIDTH, io.rowNumIn - 16, 15)
        //前15个行的数据都忽略 起始位置就是15，这两个数据描述fifo数据给出的时候他的一个状态

        //上面这个控制状态可以在写一个控制比较，思考中。。。。
        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(WAIT)
              }
          }
        //colCnt.count >= U(maxPoolingFixConfig.kernelSize - 1, log2Up(maxPoolingFixConfig.kernelSize - 1) bits)
        WAIT
          .whenIsActive {
              when(dataFire && colCnt.valid && rowCnt.count === 29 ) {//前30行都不需要注意，
                  goto(WAIT)
              }
          }
        VALID
          .whenIsActive { //这个是数据有效的阶段
              when(dataFire && colCnt.valid && rowCnt.valid) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期计数
              goto(IDLE)
          }
    }
    //生成数据，选择数据，开始计算
    //windows产生的有效数据 不是selData需要的有效数据 需要经过对比
    val sReady = Bool()
    val windows = dataGenerate(fsm.dataFire, sReady)
    //获得特征点
    val featurePoint = Vec(Stream(FeaturePoint(NMSConfig())), 2)
    inputFeaturePoint(featurePoint)
    //得到windows输出的数据，现在是对应那一个位置
    val windowsBits = Vec(windows.payload.map((x : Vec[Bits]) => {Vec(x.map((y : Bits) => {y.asUInt}))}))
    val windowsFlow = Flow(windowsBits)
    windowsFlow.payload := windowsBits
    val selDataOutput = selData(windowsFlow, featurePoint(1).selNum.asBits, featurePoint(1).ready)
    val featurePointColCntDleayWindows, featurePointRowCntDleayWindows = UInt(config.SIZE_WIDTH bits).setAsReg() init (0)
    when(fsm.dataFire) { //windows窗口的位置，左侧空闲16，右侧空闲16，指示windows产生的数据现在所处整个图像的位置
        featurePointColCntDleayWindows := fsm.featurePointColCnt.count
        featurePointRowCntDleayWindows := fsm.featurePointRowCnt.count
    }
    //因为windows窗口需要31行的数据，所以featurePoint一定是比windows所指向的位置要靠后的。
    //1、需要处理windowsFlow.valid控制selData选择合适的数据                                       windowsFlow.valid
    //2、需要处理featurePoint的ready信号，每次selData数据即将输出完毕后，拉高ready信号，让新的数据进来    featurePoint(1).ready
    //3、需要处理windwos.ready信号让窗口向后跳动                                                  windwos.ready
    val featurePointJudge = Bits(2 bits)
    featurePointJudge(0) := featurePointColCntDleayWindows === featurePoint(0).colNum && featurePointRowCntDleayWindows === featurePoint(0).rowNum
    featurePointJudge(1) := featurePointColCntDleayWindows === featurePoint(1).colNum && featurePointRowCntDleayWindows === featurePoint(1).rowNum
    windows.ready := !(featurePoint(0).valid & featurePoint(1).valid) || !(selDataOutput.valid && featurePointJudge(0))
    windowsFlow.valid := featurePoint(1).valid && !selDataOutput.valid && featurePointJudge(1)


    //数据准备完毕，进行最后的计算
    compute(selDataOutput, sReady)
}

class RSBriefOrb(config:RSBriefConfig) extends Module {
    val io = new Bundle {
        //输入信号和输出信号，确保size*size个数据同时输出
        val sDataImage = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
        val sDataFeaturePoint = slave Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH)
        val mDataRsBrief = master Stream Bits(64 bits)
        val start = in Bool()
    }

    //1、产生一个窗口数据,不进行padding
    val windows31 = new syncWindowsPadding2(config.dataGenerateRow31Config)
    windows31.io.start := io.start
    windows31.io.sizeIn := io.sizeIn
    //2、获得特征点数据，根据特征点数据的值,产生valid有效信号
    val fp1 = io.sDataFeaturePoint.m2sPipe(holdPayload = true)
    val fp2 = fp1.m2sPipe(holdPayload = true)
    //3、窗口数据的选择，根据fp2的数据获取对应的窗口数据
    val windowsCnt = ImageCount(windows31.io.mData.fire, io.sizeIn)
    windowsCnt.colCnt.count.addAttribute("MAX_FANOUT", 200)
    val rowValid = windowsCnt.rowCnt.count === fp2.payload.size.rowNum
    val colValids = Bits(config.DATA_NUM bits)
    for(sel <- 0 until  config.DATA_NUM){
        val curCnt = windowsCnt.getSize().setSel(U(sel, log2Up(config.DATA_NUM) bits))
        colValids(sel) := curCnt.colNum === fp2.payload.size.colNum
    }
    val fpWinValid = rowValid && colValids.orR && fp2.valid//特征点和数据位置对应。
    //4、选择所需数据
    val sel = OHToUInt(Vec(config.MASK4.map(mask => mask === colValids))) //选择出有效数据 得到key
    val selData0 = Flow(Vec(Vec(UInt(8 bits), 31), 31))
    for (j <- 0 to 30) { //选择不同情况的数据进行传输
        switch(sel) {
            for (i <- 0 to 7) {
                is(i) { //偏移i个值
                    for (k <- 0 to 30) {
                        selData0.payload(j)(k) := windows31.io.mData.payload(j).asBits.subdivideIn(5 * 8 slices)(k + i + 1).asUInt
                    }
                }
            }
        }
    }
    //5、生成数据有效信号，进行计算
    selData0.valid := windows31.io.mData.valid && fpWinValid
    val brief = new BRIEF(BRIEFConfig()) //1
    val icAngle = new IC_Angle(IC_AngleConfig()) //15
    val tan = new tan2(tan2Config())
    val rotate = new Rotate(rotateConfig())
    brief.io.sData <> selData0
    icAngle.io.sData <> selData0
    tan.io.sData <> icAngle.io.mData
    rotate.io.sDataTan <> tan.io.mData
    rotate.io.sDataBrief <> brief.io.mData
    //6、根据有效数据生成ready信号，控制有效节奏
    val validCnt = WaCounter(selData0.valid, 3, 7)
    fp2.ready := (selData0.valid && validCnt.valid)
    windows31.io.mData.ready := (!selData0.valid) || (selData0.valid && validCnt.valid && (fp1.valid && fp1.payload.size.notEqual(fp2.payload.size))) || !fp2.valid
    //7、缓存结果
    windows31.io.sData <> io.sDataImage.continueWhen(rotate.io.sReady)
    val fifoRsBrief = StreamFifo(Bits(64 bits), 1024)
    rotate.io.mDataRsBrief <> fifoRsBrief.io.push
    io.mDataRsBrief <> fifoRsBrief.io.pop
}


object RSBrief extends App {
    SpinalVerilog(new RSBriefOrb(RSBriefConfig())).printPruned
}