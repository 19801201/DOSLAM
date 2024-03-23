package operator

import data._
import dataStructure.{FeaturePoint, FeaturePointOrb}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}
import wa.xip.xpm._
//实现流水线模块的NMS，进行8次大小判断，得到非极大值抑制之后的结果
/*
    给出一个输入经过rowMem模块得到3行输出，经过windowsReg得到8 * 9个点的数据 经过Comparison得到是否是特征点的结果。最后结果保存在fifo中。
    然后控制反压。这个模块存在一个问题就是在计算边界的时候，边界条件需要给0。
 */
case class NMSConfig(DATA_WIDTH : Int = 8,
                     DATA_NUM : Int = 8,
                     MEM_DEPTH : Int = 128
                                    ){
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    val SIZE_WIDTH = 11
    val MEM_NUM = 2
    val WINDOWS_W = 3
    val WINDOWS_H = 3
    val x = Array(0, 1, 2, 2, 2, 1, 0, 0)
    val y = Array(2, 2, 2, 1, 0, 0, 0, 1)
    val boundaryDetection = 16

    val windows33 = WindowsConfig(DATA_NUM, WINDOWS_H, WINDOWS_W, MEM_DEPTH)
}

class NMS(config:NMSConfig) extends Module{
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mData = master Stream FeaturePoint(config)
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
    }

    def rowMem(rdData: Vec[Bits], inputData: Bits, wen: Bool, EndAddr: UInt): Unit = {
        val count = WaCounter(wen, log2Up(config.MEM_DEPTH), EndAddr)
        val rdAddr = count.count //周期1：读取数据
        val wrAddr = RegNext(rdAddr, U(0))
        val mem = Array.tabulate(config.MEM_NUM)(i => {
            def gen(): Mem[Bits] = {
                val mem = Mem(Bits(config.DATA_STREAM_WIDTH bits), wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")

                mem.write(wrAddr, rdData(i + 1), RegNext(wen)) //同步写,使能延迟
                //存入
                when(wen){
                    rdData(i) := mem.readAsync(rdAddr) //同步读
                }
                //从0到n-2的范围的MEM读出
                mem
            }

            gen()
        })
        when(wen){
            rdData(config.MEM_NUM) := inputData
        }
    }

    def windowsReg(rdData:Vec[Bits], wen:Bool): Vec[Vec[Bits]] = {
        val windows = Vec(Vec(Reg(Bits(config.DATA_STREAM_WIDTH bits)) init 0, config.WINDOWS_W ), config.WINDOWS_H)
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
        windows//得到若干行的结果
    }
    //将3*3*8大小的框变为比较所需要的数据格式类型
    def formatConversion(windows: Vec[Vec[Bits]]): Vec[Vec[Vec[Bits]]] = {
        val temp = Vec(Vec(Vec(Bits(config.DATA_WIDTH bits), config.WINDOWS_W), config.WINDOWS_H), config.DATA_NUM)

        for(i <- 0 until  config.DATA_NUM){
            for (j <- 0 until  config.WINDOWS_H) {
                for (k <- 0 until  config.WINDOWS_W) {
                    temp(i)(j)(k) := windows(j).asBits.subdivideIn(config.WINDOWS_W * config.DATA_NUM slices)(7 + i + k)
                }
            }
        }

        temp
    }

    def Comparison(compareData:Vec[Vec[Bits]]):Bool = {//中间的数大于所有的数，那么这个点可以被保留
        val isGreater = Vec(Bool(), 8)
        for(i <- 0 to 7){
            isGreater(i) := compareData(config.y(i))(config.x(i)).asUInt < compareData(1)(1).asUInt
        }
        isGreater.andR
    }
    val MemDataValid = Bool()//MemData输出的有效数据开始时
    val flow = new Area {//先写流水线，
        val valid, ready, fire = Vec(Bool(), 6)
        valid.setAsReg()
        //数据流入 MEM信号
        io.sData.ready := ready(0)
        fire(0) := io.sData.fire
        when(ready(0)){//数据位置处于有效，而且上册数据有效。
            valid(0) := io.sData.valid && MemDataValid
        }
        ready(0) := !valid(0) || ready(1)//满足任意一种情况就可以接收数据。
        //MEM的信号 窗口寄存器1
        fire(1) := valid(0) && ready(1)
        ready(1) :=  !valid(1) || ready(2)
        when(ready(1)) { //数据位置处于有效，而且上层数据有效。
            valid(1) := valid(0)
        }
        //MEM的信号 窗口寄存器2
        fire(2) := valid(1) && ready(2)
        ready(2) := !valid(2) || ready(3)
        when(ready(2)) { //数据位置处于有效，而且上层数据有效。
            valid(2) := valid(1)
        }
        //MEM的信号 窗口寄存器3 并且计算模块数据有效,同时也是计算的存储模块
        fire(3) := valid(2) && ready(3)
        ready(3) := !valid(3) || ready(4)
        when(ready(3)) { //数据位置处于有效，而且上层数据有效。
            valid(3) := valid(2)
        }


        //计算结果存储模块 储存计算模块的结果 1、计算模块的结果不一定有效 2、计算模块可能是1，2，3，4个如果是2，3，4个需要先存储数据。暂时实现的一版计算阻塞运算
        //ready信号不能传播太长的距离，使用一个滑动缓冲区，给下层的数据。这层数据按正常情况处理
        fire(4) := valid(3) && ready(4)

        //ready信号有效分为两种情况，1、valid无效，2、valid有效但是其只有一个有效数据，并且下级ready有效能够接收数据
        //sum需要
        //数据流向FIFO
        fire(5) := valid(4) && ready(5)
    }
    def flowStore(dataType: UInt, start : Int, end : Int): UInt = {//传输一个任意类型的数据，
        val data = Vec(UInt(dataType.getWidth bits), end - start).setAsReg()
        for(i <- start until end){
            if(i == start){
                when(flow.fire(i)) {
                    data(i - start) := dataType
                }
            } else {
                when(flow.fire(i)) {
                    data(i - start) := data(i - start - 1)
                }
            }
        }
        data(end - start - 1)
    }

    val fsm = new StateMachine { //
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val END = new State
        //两种情况下会输出有效数据，第一种接收到有效数据，第二种mready为真，输出有效数据

        val colCnt = WaCounter(io.sData.fire, io.colNumIn.getWidth, io.colNumIn - 1)
        val rowCnt = WaCounter(io.sData.fire && colCnt.valid, io.rowNumIn.getWidth, io.rowNumIn - 1)
        MemDataValid := rowCnt.count > 1 //第二行之后的数据有效
        val firstCol, endCol = Reg(Bool()) init False

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
    //对保存的数据进行计算
    val determineMaximumValuePoint = new Area {
        val maximumPoint = Vec(Bool().setAsReg(), config.DATA_NUM)//判断是否是极大值的结果
        val sornerScore = Vec(Bits(config.DATA_WIDTH bits).setAsReg(), config.DATA_NUM)
        val rdData = Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_H).setAsReg()
        rowMem(rdData, io.sData.payload, flow.fire(0), io.colNumIn - 1)//这里是一个FIFO
        val windows = windowsReg(rdData, flow.fire(1)): Vec[Vec[Bits]]//这里是下一个FIFO，不仅是FIFO而且是第二次输入得到的结果才是有效的
        val compareData = formatConversion(windows)//
        for(i <- 0 until config.DATA_NUM){//有些点被NMS清除
            when(flow.fire(3)){
                maximumPoint(i) := Comparison(compareData(i))
                sornerScore(i) := windows(1)(1).subdivideIn(8 slices)(i)
            }
        }
    }
    //将结果储存，交给下一层进行分发,上一层的结果暂存交给这一层进行处理
    val store = new Area{//在这里组织成一个数据结构 需要位置坐标，x : 10, z : 3, y : 11,  light 8
        val featurePointValue = Vec(FeaturePoint(config), 4).setAsReg()//定义数据结构,将这一层的结构全部存放在这里
        val storeValidReg = Vec(Reg(Bool()) init False, config.DATA_NUM/2)
        val storeValid = Vec(Bool(), config.DATA_NUM/2)//这里给出4个REG内的数据是否有效
        val storeSel = Vec(Bits(2 bits), config.DATA_NUM/2)//特征点的选择，是否有特征点
        val colCnt = flowStore(fsm.colCnt.count, 0, 4)//特征点所在的位置，延迟4个周期（MEM,WINDOWS1,DINDOWS2,COMPUTE）
        val rowCnt = flowStore(fsm.rowCnt.count, 0, 4)//特征点所在的位置，延迟4个周期（MEM,WINDOWS1,DINDOWS2,COMPUTE）
        val colCntValid = flowStore(fsm.colCnt.valid.asUInt, 0, 4).asBool
        //val colCnt = fsm.colCnt.count
        //val rowCnt = fsm.rowCnt.count
        //val colCntValid = fsm.colCnt.valid
        val tempMaximumPoint = Vec(Bool(), config.DATA_NUM)
        storeSel := determineMaximumValuePoint.maximumPoint.asBits.subdivideIn(config.DATA_NUM/2 slices)

        tempMaximumPoint(0) := !colCnt.orR && determineMaximumValuePoint.maximumPoint(0)//如果是第0个，那么这个数据一定无效
        tempMaximumPoint(7) := !colCntValid && determineMaximumValuePoint.maximumPoint(0)
        for(i <- 1 to 6)(
          tempMaximumPoint(i) := determineMaximumValuePoint.maximumPoint(i)
        )
        val curValidSum = ((storeValid(0).asUInt +^ storeValid(1).asUInt) +^ (storeValid(2).asUInt +^ storeValid(3).asUInt))
        val surplusValidsum = Reg(UInt(curValidSum.getWidth bits)) init 0

        when(flow.ready(4)) { //分为两种状态，如果是第一次接收数据valid有效，并且计算结果有效，如果是第二次接收数据只要求包含有效数据
            flow.valid(4) := (flow.valid(3) && storeValid.orR)//这里有有效数据
        }//当接收到数据就让valid信号一直有效，知道所有的数据都被fifo接收
        flow.ready(4) := !flow.valid(4) || (flow.ready(5) && surplusValidsum <= 1)//这里是fifo给的,满足这个条件ready是有效的可以接收数据。
        when(flow.fire(4)){
            surplusValidsum := curValidSum
            storeValidReg := storeValid
        } elsewhen(flow.fire(5)){
            surplusValidsum := surplusValidsum - 1
            storeValidReg := storeValidReg //默认结果
            switch(storeValidReg.asBits){
                is(M"1000"){
                    storeValidReg(3) := False
                }
                is(M"-100"){
                    storeValidReg(2) := False
                }
                is(M"--10"){
                    storeValidReg(1) := False
                }
                is(M"---1"){
                    storeValidReg(0) := False
                }
                default{
                    storeValidReg.foreach(_ := False)
                }
            }
        }
        //本次计算的有效数据量，
        //curValidSum代表当前剩余的有效个数，分别为0，1，2，3，4，如果是
        for(i <- 0 until 4){
            when(flow.fire(4)){
                featurePointValue(i).colNum := colCnt.resized
                featurePointValue(i).rowNum := (rowCnt - 1).resized
            }
            storeValid(i) := storeSel(i).orR//其中一个为真即可
            when(flow.fire(4)) {
                when(curValidSum > 0){
                    featurePointValue(i).num := (curValidSum - 1).resized
                } otherwise {
                    featurePointValue(i).num := 0
                }
            }

            switch(storeSel(i)){
                is(1){
                    when(flow.fire(4)){
                        featurePointValue(i).dataPoint := determineMaximumValuePoint.sornerScore(2 * i).asUInt
                        featurePointValue(i).selNum := ( 2 * i )
                    }
                }
                is(2){
                    when(flow.fire(4)) {
                        featurePointValue(i).dataPoint := determineMaximumValuePoint.sornerScore(2 * i + 1).asUInt
                        featurePointValue(i).selNum := ( 2 * i ) + 1
                    }
                }
                default{
                    when(flow.fire(4)) {
                        featurePointValue(i).dataPoint := 0
                        featurePointValue(i).selNum := 0
                    }
                }
            }
        }
    }

    val distribute = new Area{
        //val fifo = FifoSync(XPM_FIFO_SYNC_CONFIG(MEM_TYPE.block, 1, FIFO_READ_MODE.std, 2048, 32, 32))//本身是流水线5
        val fifo = StreamFifo(store.featurePointValue(0), 2048)
        fifo.io.pop <> io.mData
        fifo.io.push.valid := flow.valid(4)//这个数据有效代表可以传递有效数据
        flow.ready(5) := fifo.io.push.ready
        //val a = B"32'1"
        switch(store.storeValidReg.asBits) {
            is(M"1000") {
                fifo.io.push.payload := store.featurePointValue(3)
            }
            is(M"-100") {
                fifo.io.push.payload := store.featurePointValue(2)
            }
            is(M"--10") {
                fifo.io.push.payload := store.featurePointValue(1)
            }
            is(M"---1") {
                fifo.io.push.payload := store.featurePointValue(0)
            }
            default {
                fifo.io.push.payload := store.featurePointValue(0)
            }
        }
    }
}

class NMS1(config:NMSConfig) extends Module {
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mData = master Stream new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH)
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
        val done = out Bool()
    }
    //1、首先生成3行3列的数据。然后比较结果是否满足
    //得到根据生成的满足结果，产生mask码，选择这一位结果
    //不断产生数据
    //1、产生3行数据
    val windows = new syncWindowsPadding2(config.windows33)
    windows.io.start := io.start
    windows.io.sizeIn := io.sizeIn
    windows.io.sData <> io.sData

    val MASK4 = Array(
        M"-------1",
        M"------10",
        M"-----100",
        M"----1000",
        M"---10000",
        M"--100000",
        M"-1000000",
        M"10000000")

    val MASK = Array(
        M"00000001",
        M"0000001-",
        M"000001--",
        M"00001---",
        M"0001----",
        M"001-----",
        M"01------",
        M"1-------")

    val MASK2 = Array(
        M"1-------",
        M"01------",
        M"001-----",
        M"0001----",
        M"00001---",
        M"000001--",
        M"0000001-",
        M"00000001")

    //2、生成比较结果
    val formatData = formatConversion(windows.io.mData.payload, config.DATA_NUM)
    val maxPointValid = Vec(formatData.map(data => Comparison(data, config.y, config.x))).asBits
    val maxPointValid_r = RegNextWhen(maxPointValid, windows.io.mData.fire, B(0, config.DATA_NUM bits))
    val scorePoint = Vec(formatData.map(data => data(1)(1)))
    val scorePoint_r = RegNextWhen(scorePoint, windows.io.mData.fire)
    printf("scorePoint_r:%d scorePoint_r.head.getWidth%d\n",scorePoint_r.size, scorePoint_r.head.getWidth)
    val cnt = ImageCount(windows.io.mData.fire, windows.io.sizeOut)
    //3、产生，从maxPointValid中按顺序选择出有效数据
    val mask = Reg(Bits(config.DATA_NUM bits)) init(0)
    val curMaxPointValid = mask ^ maxPointValid_r
    val key = OHToUInt(Vec(MASK4.map(mask => mask === curMaxPointValid)))//选择出有效数据 得到key
    when(windows.io.mData.fire){
        mask := B(0, config.DATA_NUM bits)
    } elsewhen(curMaxPointValid.orR){
        mask :=  mask | UIntToOh(key)
    }
    //4、保存新产生的数据
    val fifo = StreamFifo(new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH), 1024)
    fifo.io.push.payload := new FeaturePointOrb(config.SIZE_WIDTH, config.DATA_WIDTH, cnt.getSize().regNextWhen(windows.io.mData.fire).setSel(key), scorePoint_r.read(key).asUInt)
    fifo.io.push.valid := curMaxPointValid.orR
    fifo.io.pop <> io.mData
    val fifoReady = RegNext(fifo.io.availability > 16)
    windows.io.mData.ready := fifoReady && (curMaxPointValid.subdivideIn(config.DATA_NUM slices).map(p => p.asUInt).reduceBalancedTree(_ +^ _) <= U"3'b1")

    val done = RegInit(False) setWhen(cnt.fireCnt) clearWhen (io.start)
    io.done := (Delay(done, 2) && !fifo.io.pop.valid).rise(False)
}

object NMS extends App {
    SpinalVerilog(new NMS(NMSConfig()))
}

object NMS1 extends App {
    SpinalVerilog(new NMS1(NMSConfig()))
}