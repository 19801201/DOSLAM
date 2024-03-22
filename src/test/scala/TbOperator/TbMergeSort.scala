package TbOperator

import TbOperator.TbSort1.spinalConfig
import TbTop.TbORB_Compute.spinalConfig
import spinal.core._
import spinal.core.sim._
import operator.{MergeA, MergeSort, MergeSortConfig}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
case class TbSort(config : MergeSortConfig) extends MergeSort(config){
  val random = new Random()
  val randData = (1 to config.maxNum).map(_ => random.nextInt())
  val sortData = randData.sorted
  //randData.sorted.reverse
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

  def init = {
    clockDomain.forkStimulus(5000)

    io.idi.valid #= false
    io.idi.payload #= 0

    io.ido.ready #= false
    clockDomain.waitSampling(10)
  }

  def in(src: String): Unit = {
    val random = new Random()
    fork {
      for (line <- randData) {
        io.idi.payload #= line
        io.idi.valid #= true
        clockDomain.waitSamplingWhere(io.idi.valid.toBoolean && io.idi.ready.toBoolean)
        io.idi.valid #= false
        val randomInt = random.nextInt(25)
        if(randomInt < 4)  clockDomain.waitSampling(randomInt)
      }
      io.idi.valid #= false
    }
  }

  def out(dst_scala: String, dst: String): Unit = {
    clockDomain.waitSampling()

    var error = 0
    var iter = 0
    var i = 0
    while (i < config.maxNum) {
      clockDomain.waitSampling()
      io.ido.ready.randomize()
      //io.ido.ready #= true
      if (io.ido.valid.toBoolean && io.ido.ready.toBoolean) {
        //io.ic.start #= false
        val temp = sortData(i)
        //val o = toHexString(8, io.ido.payload.toBigInt)
        i = i + 1
        if (!temp.equals(io.ido.payload.toInt)) {
          error = error + 1
          println("i:" + i + "data:" + io.ido.payload.toInt)
        }
        if (iter % 1000 == 999) {
          val errorP = error * 100.0 / config.maxNum
          println(s"total iter = $config.maxNum current iter =  $iter :::  error count = $error error percentage = $errorP%")
        }
        iter = iter + 1
      }
    }
    if (error > 0) {
      println(s"error is $error\n")
    } else {
      println(s"ac\n")
    }
    println()
    clockDomain.waitSampling(1000)
    simSuccess()
  }
}

case class TbTopSort() extends MergeA{
  val random = new Random()
  val data = (1 to 10).map(_ =>{
    (1 to 64).map(_ => random.nextInt()).sorted
  })
  val sortData = data.flatten.sorted.take(64)
  //randData.sorted.reverse
  println("randdata")
  for((data1, i) <- data.zipWithIndex){
    for((data2,j) <- data1.zipWithIndex){
      println(i + "," + j + ":" + data2)
    }
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

  def init = {
    clockDomain.forkStimulus(5000)

    io.input.valid #= false
    io.input.payload #= 0

    io.output.ready #= false
    io.enLastVec #= false

    clockDomain.waitSampling(10)
  }

  def in(src: String): Unit = {
    val random = new Random()
    fork {
      var i = 0
      for (line <- data.flatten) {
        i = i + 1
        //println(i)
        if(i == 9 * 64) io.enLastVec #= true
        else io.enLastVec #= false

        io.input.payload #= line
        io.input.valid #= true
        clockDomain.waitSamplingWhere(io.input.valid.toBoolean && io.input.ready.toBoolean)
        io.input.valid #= false
        val randomInt = random.nextInt(25)
        if(randomInt < 4)  clockDomain.waitSampling(randomInt)
      }
      io.input.valid #= false
    }
  }

  def out(dst_scala: String, dst: String): Unit = {
    clockDomain.waitSampling()

    var error = 0
    var iter = 0
    var i = 0
    while (i < 64) {
      clockDomain.waitSampling()
      //io.output.ready.randomize()
      io.output.ready #= true
      if (io.output.valid.toBoolean && io.output.ready.toBoolean) {
        //io.ic.start #= false
        val temp = sortData(i)
        //val o = toHexString(8, io.ido.payload.toBigInt)
        i = i + 1
        if (!temp.equals(io.output.payload.toInt)) {
          error = error + 1
          println("i:" + i + "data:" + io.output.payload.toInt)
        }
        if (iter % 1000 == 999) {
          val errorP = error * 100.0 / 64
          println(s"total iter = 64 current iter =  $iter :::  error count = $error error percentage = $errorP%")
        }
        iter = iter + 1
      }
    }
    if (error > 0) {
      println(s"error is $error\n")
    } else {
      println(s"ac\n")
    }
    println()
    clockDomain.waitSampling(1000)
    simSuccess()
  }
}


object TbSort extends App {
  val spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
  )
  //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
  SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbSort(MergeSortConfig(1024))).doSimUntilVoid { dut =>
    println(log2Up(640))
    dut.init
    //dut.io.ic.start #= true
    dut.clockDomain.waitSampling(100)
    val path = ".\\data\\GaussianBlur"

    dut.in(path + "\\NUC_i.coe")
    dut.out(path + "\\dstDataOut.txt",path + "\\local_mean.coe")
  }
}

object TbSort1 extends App {
  val spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
  )
  //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
  SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbTopSort()).doSimUntilVoid { dut =>
    dut.init
    //dut.io.ic.start #= true
    dut.clockDomain.waitSampling(100)
    val path = ".\\data\\GaussianBlur"

    dut.in(path + "\\NUC_i.coe")
    dut.out(path + "\\dstDataOut.txt",path + "\\local_mean.coe")
  }
}