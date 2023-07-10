package data

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

/*
做1级流水得windows窗口，输入经过一级寄存器之后直接输出。
 */
class syncWindows(config : WindowsConfig) extends Component{
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream (Bits(config.DATA_STREAM_WIDTH bits))
        val mData = master Stream (Vec(Vec(Bits(config.DATA_STREAM_WIDTH bits),config.WINDOWS_SIZE_W), config.WINDOWS_SIZE_H))
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        //输入通道大小，
        val rowNumOut = out UInt (config.SIZE_WIDTH bits)
        val colNumOut = out UInt (config.SIZE_WIDTH bits)
    }
    //1、MEM存储数据得行阵列，wen有效+上升沿 读取rdData得数据，并输出一行得数据
    //2、如果wen信号不变那么rdData不能改变，
    def rowMem(rdData: Vec[Bits], inputData: Bits, wen: Bool, EndAddr: UInt): Unit = {
        val count = WaCounter(wen, log2Up(config.MEM_DEPTH), EndAddr)
        val rdAddr = count.count //周期1：读取数据
        val wrAddr = RegNext(rdAddr, U(0))
        val mem = Array.tabulate(config.MEM_NUM)(i => {
            def gen(): Mem[Bits] = {
                val mem = Mem(Bits(config.DATA_STREAM_WIDTH bits), wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")

                mem.write(wrAddr, rdData(i + 1), RegNext(wen)) //同步写,使能延迟
                //存入
                rdData(i) := mem.readSync(rdAddr, wen) //同步读
                //从0到n-2的范围的MEM读出
                mem
            }

            gen()
        })
        rdData(config.MEM_NUM) := RegNextWhen(inputData, wen)
    }
    //2、MEM存储数据得窗口阵列，wen有效+上升沿 移动窗口数据

    /*
        rdData来的是最新一行的数据，这个数据需要放到窗口的最右侧，然后整个窗口的数据向左移动，数据来临之后下个周期就开始有效
     */
    def windowsReg(rdData:Vec[Bits], wen:Bool): Vec[Vec[Bits]] = {
        val windows = Vec(Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_W ), config.WINDOWS_SIZE_H)
        for (h <- 0 until config.WINDOWS_SIZE_H) {
            for (w <- 0 until config.WINDOWS_SIZE_W) {
                if (w == config.WINDOWS_SIZE_W - 1) {
                    windows(h)(w) := rdData(h)
                } else {
                    windows(h)(w).setAsReg() init (0)
                    when(wen) {
                        windows(h)(w) := windows(h)(w + 1)
                    }
                }
            }
        }
        windows
    }

    //状态机对产生数据模块进行控制
    val fsm = new StateMachine {
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val END = new State

        val rowDataValid = Bool()

        val colCnt = WaCounter(io.sData.fire, config.SIZE_WIDTH, io.colNumIn - 1)
        val rowCnt = WaCounter(io.sData.fire && colCnt.valid, config.SIZE_WIDTH, io.rowNumIn - 1)

        val ValidEnd = Reg(Bool()) init False //整个图像全部接收完毕
        when(colCnt.valid && rowCnt.valid && io.sData.fire) {
            ValidEnd := True
        }

        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive {
              when(ValidEnd && !io.mData.valid) {//图像接收完毕并且窗口 整个图像都已经被传输走
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期计数
              goto(IDLE)
              ValidEnd := False
          }
    }

    io.rowNumOut := io.rowNumIn - (config.WINDOWS_SIZE_H - 1)
    io.colNumOut := io.colNumIn - (config.WINDOWS_SIZE_W - 1)

    val rowDataValid0 = fsm.rowCnt.count >= (config.WINDOWS_SIZE_H - 1)
    val colDataValid0 = fsm.colCnt.count >= (config.WINDOWS_SIZE_W - 1)
    //修改为两级流水，一级控制row，一级控制windows，windows直接使用io.mdata
//    val rowValid,rowReady = Bool()
//    val windowsValid,windowsReady = Bool()
//
//    io.sData.ready := !rowValid || rowReady
//
//    rowValid.setAsReg() init False
//    when(io.sData.fire){//上游传递数据，那么数据取决于上游数据是否有效，
//        rowValid := rowDataValid0
//    } elsewhen (rowValid && rowReady){//下游接收数据，那么数据变为无效
//        rowValid := False
//    }
//    rowReady := !windowsValid || windowsReady
//    val colDataValid1 = RegNextWhen(colDataValid0, io.sData.fire, False)
//
//    windowsValid.setAsReg() init False
//    when(rowValid && rowReady) { //上游传递数据，那么数据取决于上游数据是否有效，
//        windowsValid := colDataValid1
//    } elsewhen (windowsValid && windowsReady) { //下游接收数据，那么数据变为无效
//        windowsValid := False
//    }
//
//    io.mData.valid := windowsValid
//    windowsReady := io.mData.ready

    io.sData.ready := !io.mData.valid || io.mData.ready
    io.mData.valid.setAsReg() init False
    when(io.sData.fire) { //上游传递数据，那么数据取决于上游数据是否有效，
        io.mData.valid := io.sData.valid && rowDataValid0 && colDataValid0
    } elsewhen (io.mData.fire) { //下游接收数据，那么数据变为无效
        io.mData.valid := False
    }

//    when(io.mData.ready){
//        io.mData.valid := io.sData.valid && rowDataValid0 && colDataValid0
//    }

    val rdData = Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_H)
    rowMem(rdData, io.sData.payload, io.sData.fire, io.colNumIn - 1)
    //io.mData.payload := windowsReg(rdData, rowValid && rowReady)
    io.mData.payload := windowsReg(rdData, io.sData.fire)
}

object syncWindows extends App {
    SpinalVerilog(new syncWindows(WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 31, WINDOWS_SIZE_W = 5, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}

