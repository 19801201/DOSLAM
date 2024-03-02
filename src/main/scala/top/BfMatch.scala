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
                      DATA_WIDTH : Int = 64,
                      MEM_DEPTH : Int = 1024,
                      SIZE_WIDTH : Int = 12
                     ) {
    val SUM_WIDTH = 8
}
//使用状态机
/**
 * BfMatch 匹配两个特征点，得到的结果为 index 和 distant
 * 特征点输入，两两匹配，需要有一个选项，
 *
 *
 */
class BfMatch(config:BfMatchConfig) extends Module {
    val io = new Bundle { //
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream Bits(config.DATA_WIDTH bits)
        val start = in Vec(Bool(), 2)
        val d_max = in UInt (config.SUM_WIDTH bits)
        // last：一整张图的所有描述子输入结束
        val last = in Bool()
        val mData = master Stream(BfMatchIO())
    }//状态机和控制信号的产生
    val last = Vec(Bool(), 4)
    val fsm = fsmIC(io.start, last)

    val ioSDataFire = io.sData.fire
    //记录input状态，输入的数据总量
    val inputCount = UInt(config.SIZE_WIDTH bits)
    /**
     * 计数器
     * RSBrie记录输入的数据总量 last使能复位
     * compute记录进入计算状态的时间 自复位，
     */
    val RSBriefCnt = WaCounter(io.sData.fire, config.SIZE_WIDTH, U(config.DATA_NUM - 1, config.SIZE_WIDTH bits))
    val computeCnt = WaCounter(fsm.isActive(fsm.COMPUTE), config.SIZE_WIDTH, inputCount)
    /**
    * last0 输入结束
    * last1 一个特征值的输入结束
    * last2 重新进入inputCompute状态
    */
    last(0) := io.last
    last(1) := io.sData.fire && fsm.isActive(fsm.COMPUTEINPUT) && RSBriefCnt.count(1 downto 0).orR
    last(2) := computeCnt.validLast()
    last(3) := io.mData.fire

    val computeOneDataClear = RegNext(fsm.isActive(fsm.COMPUTE) && !computeCnt.count(1 downto 0).orR)
    val computeOneDataLast = Delay(fsm.isActive(fsm.COMPUTE) && computeCnt.count(1 downto 0).andR, 2)
    //记录输入数据总量
    inputCount := RegNextWhen(RSBriefCnt.count, io.last, U(0))
    /**
     * 使用两个MEM，input写入mem1，inputcompute写入mem2
     * mem1：写入地址 RSBriefCnt生成 读出地址由computeCnt生成
     * mem2：写入地址 RSBriefCnt生成 读出地址由computeCnt生成
     */
    val RSBriefMem1 = Mem(Bits(config.DATA_WIDTH  bits), config.MEM_DEPTH)
    val RSBriefMem2 = Mem(Bits(config.DATA_WIDTH  bits), config.DATA_NUM)
    //写入数据
    RSBriefMem1.write(RSBriefCnt.count.resized, io.sData.payload, io.sData.fire && fsm.isActive(fsm.INPUT))
    RSBriefMem2.write(RSBriefCnt.count(1 downto 0).resized, io.sData.payload, io.sData.fire && fsm.isActive(fsm.COMPUTEINPUT))
    //读取数据
    val RSBriefData1 = RSBriefMem1.readSync(computeCnt.count.resized)
    val RSBriefData2 = RSBriefMem2.readSync(computeCnt.count(1 downto 0).resized)
    //对数据进行计算
    val sumOne = Vec(UInt(1 bits), config.DATA_WIDTH)
    sumOne.zipWithIndex.foreach(data =>  {data._1 := (RSBriefData1(data._2) ^ RSBriefData2(data._2)).asUInt})
    val sum = sumOne.reduceBalancedTree((a ,b) => {a +^ b})
    val distant = new accumulator(8, sum.resize(8), computeOneDataClear)
    //得到的结果 包括已经存在结果的标号和不在结果之内的标号。
    val curIndex1 = RegNextWhen(computeCnt.count, computeOneDataLast)
    val curIndex2 = RegNextWhen(RSBriefCnt.count, fsm.isEntering(fsm.COMPUTEINPUT))
    val BfMatchData = Stream(BfMatchIO())
    BfMatchData.valid.setAsReg()
    BfMatchData.payload.setAsReg()
    when(fsm.isEntering(fsm.COMPUTE)){
        BfMatchData.set(io.d_max)
    }
    when(computeOneDataLast && (distant.accNum < BfMatchData.distant)){
        BfMatchData.distant := distant.accNum
        BfMatchData.index(0) := curIndex1
        BfMatchData.index(1) := curIndex2
        BfMatchData.valid := True
    }
    when(BfMatchData.ready){
        BfMatchData.valid := False
    }
    val outputValid = Delay(fsm.isEntering(fsm.READY), 2)
    io.mData << BfMatchData.continueWhen(outputValid)
    //反压控制
    io.sData.ready := fsm.isActive(fsm.COMPUTEINPUT)
}

object BfMatch extends App {
    SpinalVerilog(new BfMatch(BfMatchConfig()))
}

