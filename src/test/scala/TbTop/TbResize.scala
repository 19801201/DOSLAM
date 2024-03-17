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

class TbResize(config: ResizeConfig1) extends Resize(config){
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

    //这是 经过位宽转换后输入的大小
    io.colNumFlilterIn #= ((col + 9) / 10) - 1
    //这个是真正的有效数据输入个数
    io.rowNumSrcIn #= row - 1
    io.colNumSrcIn #= ((col + 8) / 10) - 1

    var dcol = ((col / 5) * 4)
    if((col % 5) != 0)
      dcol = ((col / 5) * 4 + (col % 5) - 1)

    if(dcol % 8 == 0){
      io.mask #= BigInt("00000000", 2)
    } else {
      io.mask #= (~((1 << (dcol % 8)) - 1)) & 0xff
    }
//    io.mask #= BigInt("11111110", 2)
//    io.mask #= BigInt("00000000", 2)
    io.mData.ready #= false
    //println(s"row $row, col $col colNumIn:${io.colNumIn.toInt}, colValidNumIn:${io.colValidNumIn.toInt}, colNumSrcIn:${io.colNumSrcIn.toInt}");
    clockDomain.waitSampling(10)
  }

  def in(src: String): Unit = {
    val random = new Random()
    fork {
      for (line <- Source.fromFile(src).getLines) {
        io.sData.payload #= BigInt(line.trim, 16)
        io.sData.valid #= true
        clockDomain.waitSamplingWhere(io.sData.ready.toBoolean)
        io.sData.valid #= false
        val randomInt = random.nextInt(25)
        if (randomInt < 4) clockDomain.waitSampling(randomInt)
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
      if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
        io.start #= false
        val temp = dstFile(iter)

        val data = io.mData.payload
//        val o = toHexString(4, colNum) + toHexString(4, rowNum) + toHexString(2, data)
        val o = toHexString(16, io.mData.payload.toBigInt)
        //println(o)
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

object TbResize extends App {
  val spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
  )
  //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
  SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbResize(ResizeConfig1())).doSimUntilVoid { dut =>
    dut.init(645, 650)
    dut.io.start #= true
    dut.clockDomain.waitSampling(10)
    val path = "C:\\myData\\data\\xsim_data\\slam\\Resize650645"
    //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
    dut.in(path + "\\SourceDataIn.txt")
    dut.out(path + "\\simDataout.coe",path + "\\ReferenceDataOut.txt")
  }
}
