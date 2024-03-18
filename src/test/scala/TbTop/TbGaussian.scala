package TbTop

import operator.operator.dataGenerateImage
import operator.{GaussianBlur, GaussianBlurConfig}
import spinal.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.io.Source
import top._

import java.util.Random
import scala.collection.mutable.ArrayBuffer

class TbGaussian(config: GaussianConfig) extends Gaussian(config){
  def toHexString(width: Int, b: BigInt): String = {
    var s = b.toString(16)
    if (s.length < width) {
      s = "0" * (width - s.length) + s
    }
    s
  }

  def init(row:Int, col:Int) = {
    clockDomain.forkStimulus(5)

    io.sData.valid #= false
    io.sData.payload #= 0
    io.start #= false

    //这是真值
    io.rowNumIn #= row - 1
    io.colNumIn #= (col + 7)/8 - 1
    if(col % 8 == 0){
      io.inValid #= 7
    } else {
      io.inValid #= (col % 8) - 1
    }

    io.mData.ready #= false
    clockDomain.waitSampling(10)
  }

  def in(src: String): Unit = {
    val random = new Random()
    fork {
      for (line <- Source.fromFile(src).getLines) {
        io.sData.payload #= BigInt(line.trim, 16)
        io.sData.valid #= true
        clockDomain.waitSamplingWhere(io.sData.ready.toBoolean)
//        io.sData.valid #= false
//        val randomInt = random.nextInt(25)
//        if (randomInt < 4) clockDomain.waitSampling(randomInt)
      }
    }
  }

  def out(dst_scala: String, dst: String): Unit = {
    clockDomain.waitSampling()
    val testFile = new PrintWriter(new File(dst_scala))
    val dstFile = Source.fromFile(dst).getLines().toArray
    val total = dstFile.length
    var error = 0
    var iter = 0
    var i = 0
    while (i < dstFile.length) {
      clockDomain.waitSampling()
//      io.mData.ready #= true
      io.mData.ready.randomize()
//      if(!io.mData.ready.toBoolean){
//          clockDomain.waitSampling(8)
//      }
      if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
        io.start #= false
        val temp = dstFile(iter)

        val o = toHexString(16, io.mData.payload.toBigInt)

        if (!temp.equals(o)) {
          error = error + 1
          if(error < 100){
            printf(i + "\n");
          }
        }
        i = i + 1
        if (iter % 1000 == 999) {
          val errorP = error * 100.0 / total
          println(s"total iter = $total current iter =  $iter :::  error count = $error error percentage = $errorP%")
        }
        testFile.write(o + "\r\n")
        iter = iter + 1
      }
    }
    if (error > 0) {
      println(s"error is $error\n")
    } else {
      println(s"ac\n")
    }

    sleep(100)
    testFile.close()
    simSuccess()
  }
}

object TbGaussian extends App {
  val spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
  )
  //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
  SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbGaussian(GaussianConfig())).doSimUntilVoid { dut =>
    dut.init(480, 640)
    dut.io.start #= true
    dut.clockDomain.waitSampling(10)
    val path = "C:\\myData\\data\\xsim_data\\slam\\GaussianBlur"
    //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
    dut.in(path + "\\SourceDataIn.txt")
    dut.out(path + "\\simDataout.coe",path + "\\ReferenceDataOut.txt")
  }
}
