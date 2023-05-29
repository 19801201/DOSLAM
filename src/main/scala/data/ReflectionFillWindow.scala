package data

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

case class ReflectionFillWindowConfig(DATA_NUM : Int = 8,
                         MEM_DEPTH : Int = 1024,
                         SIZE_WIDTH : Int = 11
                        ) {
    val WINDOWS_SIZE_H = 7
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    val MEM_NUM = WINDOWS_SIZE_H - 1
    //创建的MEM个数
}
/*
    本模块只负责输出7行数据，接受到fire之后，下个时刻即给出数据
 */
class ReflectionFillWindow(config:ReflectionFillWindowConfig) extends Module{


    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH bits)
        val mData = master Flow Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_H)
        val mReady = in Bool()
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
    }


    //输入一个数，下个周期读出前n行对应位置的数据
    //写使能与写地址同步即可
    def rowMem(rdData:Vec[Bits], inputData:Bits , wen:Bool, EndAddr: UInt): Unit = {
        val count = WaCounter(wen, log2Up(config.MEM_DEPTH), EndAddr)
        val rdAddr = count.count //周期1：读取数据
        val wrAddr = RegNext(rdAddr, U(0))
        val mem = Array.tabulate(config.MEM_NUM)(i => {
            def gen(): Mem[Bits] = {
                val mem = Mem(Bits(config.DATA_STREAM_WIDTH bits), wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")

                mem.write(wrAddr, rdData(i + 1), RegNext(wen))//同步写,使能延迟
                //存入
                rdData(i) := mem.readSync(rdAddr)//同步读
                //从0到n-2的范围的MEM读出
                mem
            }

            gen()
        })
        rdData(config.MEM_NUM) := RegNext(inputData)
    }

    val fsm = new StateMachine {//分为四个状态，空闲，有效，和输出down的数据，END
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val DOWN = new State
        val END = new State
        //两种情况下会输出有效数据，第一种接收到有效数据，第二种mready为真，输出有效数据
        val cen = (isActive(VALID) && io.sData.fire) || (isActive(DOWN) && io.mReady)

        val colCnt = WaCounter(cen, io.colNumIn.getWidth, io.colNumIn - 1)
        val rowCnt = WaCounter(io.sData.fire && colCnt.valid, io.rowNumIn.getWidth, io.rowNumIn - 1)
        val downCnt = WaCounter(isActive(DOWN) && io.mReady && colCnt.valid, 2, 2)

        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive { //行列都为0
              when(io.sData.fire && colCnt.valid && rowCnt.valid) {
                  goto(DOWN)
              }
          }
        DOWN
          .whenIsActive {
              when(io.mReady && colCnt.valid && downCnt.valid) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期计
              goto(IDLE)
          }
    }

    val rdData = Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_H)
    rowMem(rdData, io.sData.payload, fsm.cen, io.colNumIn - 1)
    io.sData.ready := fsm.isActive(fsm.VALID) && io.mReady//这个数据有效时才进数据
    io.mData.valid := RegNext((fsm.isActive(fsm.VALID) && io.sData.fire && fsm.rowCnt.count > 2) || (fsm.isActive(fsm.DOWN) && io.mReady))

    switch(RegNext(fsm.rowCnt.count)){
        is(3){
            io.mData.payload(0) := rdData(6)
            io.mData.payload(1) := rdData(5)
            io.mData.payload(2) := rdData(4)
        }
        is(4){
            io.mData.payload(0) := rdData(4)
            io.mData.payload(1) := rdData(3)
            io.mData.payload(2) := rdData(2)
        }
        is(5){
            io.mData.payload(0) := rdData(2)
            io.mData.payload(1) := rdData(1)
            io.mData.payload(2) := rdData(2)
        }
        default{
            io.mData.payload(0) := rdData(0)
            io.mData.payload(1) := rdData(1)
            io.mData.payload(2) := rdData(2)
        }
    }
    io.mData.payload(3) := rdData(3)
    when(RegNext(fsm.isActive(fsm.DOWN))){
        switch(RegNext(fsm.downCnt.count)) {
            is(0) {
                io.mData.payload(4) := rdData(4)
                io.mData.payload(5) := rdData(5)
                io.mData.payload(6) := rdData(4)
            }
            is(1) {
                io.mData.payload(4) := rdData(4)
                io.mData.payload(5) := rdData(3)
                io.mData.payload(6) := rdData(2)
            }
            is(2) {
                io.mData.payload(4) := rdData(2)
                io.mData.payload(5) := rdData(1)
                io.mData.payload(6) := rdData(0)
            }
            default {
                io.mData.payload(4) := rdData(4)
                io.mData.payload(5) := rdData(5)
                io.mData.payload(6) := rdData(6)
            }
        }
    } otherwise{
        io.mData.payload(4) := rdData(4)
        io.mData.payload(5) := rdData(5)
        io.mData.payload(6) := rdData(6)
    }
}

object ReflectionFillWindow extends App {
    SpinalVerilog(new ReflectionFillWindow(ReflectionFillWindowConfig()))
}