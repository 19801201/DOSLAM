package top
import data.{ReflectionFillWindow, ReflectionFillWindowConfig, WindowsConfig, formatCornerScoreWindos, syncWindowsPadding}
import dataStructure.{BfMatchIO, FeaturePoint, FeaturePointOrb}
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.{BootCount, WaCounter}
import operator._
import spinal.core.Component.push
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize, accumulator, fsmIC}

case class BfMatchConfig(DATA_NUM : Int = 4,
                      DATA_WIDTH : Int = 32,
                      MEM_DEPTH : Int = 1024,
                      SIZE_WIDTH : Int = 11
                     ) {
    val SUM_WIDTH = 8
}
//使用状态机
class BfMatch(config:BfMatchConfig) extends Module {
    val io = new Bundle { //
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_WIDTH bits)
        val start = in Vec(Bool())
        val d_max = in UInt (config.SUM_WIDTH bits)
        val last = in Bool()
        val mData = master Stream(BfMatchIO())
    }//先处理输入状态
    val last = Vec(Bool(), 4)
    val fsm = fsmIC(io.start, last)//开始与结束信号
    //输入的有效数据
    val inputCV = io.sData.fire && fsm.isActive(fsm.COMPUTE)
    val inputIV = io.sData.fire && fsm.isActive(fsm.INPUT)
    //读写状态控制
    val writeCnt = WaCounter(io.sData.fire, config.SIZE_WIDTH)
    val rsBriefMem = Mem(Bits(32 bits), config.MEM_DEPTH)
    rsBriefMem.write(writeCnt.count.reversed, io.sData.payload, inputIV)
    val inputNum = RegNextWhen(writeCnt.count, fsm.isEntering(fsm.IDLE), U(0))
    val readCnt = new BootCount(inputCV, config.SIZE_WIDTH, inputNum)
    val readData1 = rsBriefMem.readSync(readCnt.count, True)
    //
    val inputData = Mem(Bits(32 bits), 4)
    inputData.write(writeCnt.count.reversed, io.sData.payload, inputCV)
    val readData2 = inputData.readSync(readCnt.count.reversed, True)
    val sumOne = Vec(UInt(1 bits), 32)
    sumOne.zipWithIndex.foreach(data =>  {data._1 := (readData1(data._2) ^ readData2(data._2)).asUInt})
    val sum = sumOne.reduceBalancedTree((a ,b) => {a +^ b})
    val oneLast = readCnt.count(1 downto 0).orR
    val acc = new accumulator(8, sum,oneLast)
    //处理结果
    val result = RegInit(BfMatchIO())
}

