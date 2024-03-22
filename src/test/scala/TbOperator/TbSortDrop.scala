package TbOperator

import TbOperator.TbSort1.spinalConfig
import TbTop.TbORB_Compute.spinalConfig
import spinal.core._
import spinal.core.sim._
import operator.{MergeA, MergeSort, MergeSortConfig, SortDrop}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
//随机数输入的长度，最大的排序长度，取出topNum个数据，
case class TbSortDrop(randDataInputLen:Int, maxLen:Int,topNum : Int) extends SortDrop(SInt(32 bits), maxLen, (left: SInt, right: SInt) => left > right){
  val random = new Random()
  val randData = (1 to randDataInputLen).map(_ => random.nextInt())
  val sortData = randData.sorted.reverse

  println("randdata")
  for((data, i) <- randData.zipWithIndex){
    println(i + ":" + data)
  }
  println("sortdata")
  for ((data, i) <- sortData.zipWithIndex) {
    println(i + ":" + data)
  }

  def toHexString(width: Int, b: BigInt): String = {
    var s = b.toString(16)
    if (s.length < width) {
      s = "0" * (width - s.length) + s
    }
    s
  }

  def init() = {
    clockDomain.forkStimulus(5000)

    io.sData.valid #= false
    io.sData.payload #= 0

    io.mData.ready #= false

    io.topNum #= topNum
    io.flush #= false
    io.done #= false
    io.defaultData #= sortData.last

    clockDomain.waitSampling(10)
  }

  def in(): Unit = {
    val random = new Random()
    fork {
      for (line <- randData) {
        io.sData.payload #= line
        io.sData.valid #= true
        clockDomain.waitSamplingWhere(io.sData.valid.toBoolean && io.sData.ready.toBoolean)
        io.sData.valid #= false
        val randomInt = random.nextInt(25)
        if(randomInt < 4)  clockDomain.waitSampling(randomInt)
      }
      io.sData.valid #= false

      clockDomain.waitSampling(20)
      io.done #= true
      clockDomain.waitSampling(1)
      io.done #= false
    }
  }

  def out(): Unit = {
    clockDomain.waitSampling()

    var error = 0
    var iter = 0
    var i = 0
    val total = math.min(topNum, randData.length)
    while (i < total) {
      clockDomain.waitSampling()
      io.mData.ready.randomize()
      //io.ido.ready #= true
      if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
        //io.ic.start #= false
        val temp = sortData(i)
        //val o = toHexString(8, io.ido.payload.toBigInt)
        i = i + 1
        if (!temp.equals(io.mData.payload.toInt)) {
          error = error + 1
          println("i:" + i + "data:" + io.mData.payload.toInt)
        }
        if (iter % 1000 == 999) {
          val errorP = error * 100.0 / total
          println(s"total iter = $total current iter =  $iter :::  error count = $error error percentage = $errorP%")
        }
        iter = iter + 1
      }
    }
    if (error > 0) {
      println(s"error is $error\n")
    } else {
      println(s"ac\n")
    }
    clockDomain.waitSampling(1000)
    io.flush #= true
    clockDomain.waitSampling(1000)
    simSuccess()
  }
}

object TbSortDrop extends App {
  val spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
  )
  val randDataInputLen = 3100
  val maxLen = 256
  val topNum = 120
  SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbSortDrop(randDataInputLen, maxLen, topNum)).doSimUntilVoid { dut =>
    dut.init
    dut.clockDomain.waitSampling(100)

    dut.in()

    dut.out()

  }
}