//package data
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import wa.WaCounter
////需要支持4 pixels per frame和8
//
//
////根据这个模块产生不同大小的输出例如3 3 7 7都可以实现
//case class DataGenerateWindowsConfig(DATA_NUM : Int = 10,
//                         MEM_DEPTH : Int = 1024,
//                         SIZE_WIDTH : Int = 11
//                        ) {
//    val WINDOWS_H = 7
//    val WINDOWS_W = 3
//    val inValidWidth = 3
//    val DATA_WIDTH = 8
//    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
//    val MEM_NUM = WINDOWS_H - 1//创建h-1个MEM既可以保存H行数
//    val windowsConfig = WindowsConfig(DATA_NUM, WINDOWS_H, WINDOWS_W, MEM_DEPTH, SIZE_WIDTH)
//}
//
//
//class DataGenerateWindows(config: DataGenerateWindowsConfig) extends Module{//所有的资源使用都是根据配置参数生成的。
//
//    /*
//        当wen使能的时候认为inputData是有效的，并且会输出n行同一位置的有效数据
//     */
//    def rowMem(rdData:Vec[Bits], inputData:Bits , wen:Bool, EndAddr: UInt): Unit = {//输入一个数，输出n个数
//        val count = WaCounter(wen, log2Up(config.MEM_DEPTH), EndAddr)
//        val rdAddr = count.count //周期1：读取数据
//        val wrAddr = RegNextWhen(rdAddr, wen, U(0))
//        val mem = Array.tabulate(config.MEM_NUM)(i => {
//            def gen(): Mem[Bits] = {
//                val mem = Mem(Bits(config.DATA_STREAM_WIDTH bits), wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")
//
//                mem.write(wrAddr, rdData(i + 1), wen)//写
//                //存入
//                rdData(i) := mem.readAsync(rdAddr)//读
//                //从0到n-2的范围的MEM读出
//                mem
//            }
//
//            gen()
//        })
//        rdData(config.MEM_NUM) := inputData
//    }
//    /*
//        负责对h个数据暂缓w次，当wen有效时认为rdData有效，这时会暂缓h个有效的wh
//     */
//    def windowsReg(rdData:Vec[Bits], wen:Bool): Vec[Vec[Bits]] = {
//        val windows = Vec(Vec(Reg(Bits(config.DATA_STREAM_WIDTH bits)) init 0, config.WINDOWS_W ), config.WINDOWS_H)
//        for (h <- 0 until config.WINDOWS_H) {
//            for (w <- 0 until config.WINDOWS_W) {
//                if (w == config.WINDOWS_W - 1) {
//                    when(wen) {
//                        windows(h)(w) := rdData(h)
//                    }
//                } else {
//                    when(wen) {
//                        windows(h)(w) := windows(h)(w + 1)
//                    }
//                }
//            }
//        }
//        windows
//    }
//    /*
//
//     */
//
//    val io = new Bundle {

//        //增加运行速度，一次传输多个个数据
//        val sData = slave Stream (Bits(config.DATA_STREAM_WIDTH bits))
//        val mData = master Stream (Vec(Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_W), config.WINDOWS_H))
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
//        val colNumIn = in UInt (config.SIZE_WIDTH bits)
//        val inValidNum = in UInt (config.inValidWidth)
//        //输入图像的大小
//        val rowNumOut = out UInt (config.SIZE_WIDTH bits)
//        val colNumOut = out UInt (config.SIZE_WIDTH bits)
//    }
//
//    val fsm = new StateMachine {
//        setEncoding(binaryOneHot)
//        val IDLE = new State with EntryPoint //up down根据计数器来确定，
//        val LEFT = new State
//        val RIGHT = new State
//        val DOWN = new State
//        val VALID = new State
//        val END = new State
//        //一共有三个状态IDLE准备状态，接受到start信号上升沿开始工作进入valid状态
//        //VALID 工作状态，只有valid状态才正常工作 当接受完所有数据，并且所有数据都已经输出那么进入END状态
//        //END状态，
//        val rowDataValid = Bool()
//        //在这里进行输入控制。a::一定是每来一个数据才增加1
//        //一次输入八个数据这样可以更快的计算
//        val colCnt = WaCounter(io.sData.fire , config.SIZE_WIDTH, io.colNumIn - 1)//开始和结束
//        val rowCnt = WaCounter(io.sData.fire && colCnt.valid, config.SIZE_WIDTH, io.rowNumIn - 1)
//
//        val colCntM = WaCounter(io.mData.fire , config.SIZE_WIDTH, io.colNumIn - 1)//开始和结束
//        val rowCntM = WaCounter(io.mData.fire && colCnt.valid , config.SIZE_WIDTH, io.rowNumIn - 1)
//        //流水线最后一个数据接收到的位置，根据这个位置判断当前计算结果是否满足需求
//        val ValidEndT = Reg(Bool()) init False
//        val ValidEnd = Reg(Bool()) init False
//        when(colCnt.valid && rowCnt.valid && io.sData.fire) {
//            ValidEndT := True
//        } //代表正在接受最后一个数据，下个周期就表示数据已经全部被接收了
//        ValidEnd := ValidEndT //拉高时所有数据都已经接收完了。
//        //数据全部输出后拉高
//        //状态跳转
//        IDLE
//          .whenIsActive {
//              when(io.start.rise()) {
//                  goto(LEFT)
//              }
//          }
//        //colCnt.count >= U(maxPoolingFixConfig.kernelSize - 1, log2Up(maxPoolingFixConfig.kernelSize - 1) bits)
//        LEFT
//          .whenIsActive { //行列都为0.并且模块内部数据都
//              when(io.sData.valid && DataReady) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
//                  goto(VALID)//正常给数据
//              }
//          }
//        VALID
//          .whenIsActive {//在这里的数都需要有一个判断，判断条件就是满足要求的值
//              when(colCnt.valid && io.sData.fire) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
//                  goto(RIGHT) //正常给数据
//              }
//          }
//        RIGHT
//          .whenIsActive { //行列都为0.并且模块内部数据都
//              when(rightValid) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
//                  when()
//                  goto(END)
//              }
//          }
//        DOWN
//          .whenIsActive { //在这里的数都需要有一个判断，判断条件就是满足要求的值
//              when(colCnt.valid && io.sData.fire) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
//                  goto(RIGHT) //正常给数据
//              }
//          }
//        END
//          .whenIsActive { //使用一个周期计数
//              goto(IDLE)
//          }
//    }
//
//
//
//
//
//
//}
