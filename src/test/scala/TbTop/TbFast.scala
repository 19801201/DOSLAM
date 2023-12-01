package TbTop

import operator.operator.dataGenerateImage
import operator.{GaussianBlur, GaussianBlurConfig}
import spinal.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.io.Source
import top._

import scala.util.Random
case class TbFast(config : FastConfig) extends Fast(config){
    def toHexString(width: Int, b: BigInt): String = {
        var s = b.toString(16)
        if (s.length < width) {
            s = "0" * (width - s.length) + s
        }
        s
    }

    def init = {
        clockDomain.forkStimulus(5)

        io.sData.valid #= false
        //io.sData.payload #= 0
        io.start #= false
        io.rowNumIn #= 480
        io.colNumIn #= 640 >> 3
        io.threshold #= 40
        io.mData.ready #= false
        clockDomain.waitSampling(10)
    }

    def in(src: String): Unit = {
        val random = new Random()
        fork {
            for (line <- Source.fromFile(src).getLines) {
                //io.sData.payload #= BigInt(line.trim, 16)
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
            io.mData.ready #= true
            if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
                io.start #= false
                val temp = dstFile(iter)
                val colNum = io.mData.payload.colNum.toInt * 8 + io.mData.payload.selNum.toInt
                val rowNum = io.mData.payload.rowNum.toInt
                val data = io.mData.payload.dataPoint.toInt
                val o = toHexString(4, colNum) + toHexString(4, rowNum) + toHexString(2, data)

                if (!temp.equals(o)) {
                    error = error + 1
                    if(error < 100){
                        printf(i + "\n");
                    }
                }
                i = i + 1
                if (iter % 1000 == 0) {
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

case class TbFastOrb1(config : FastConfig) extends FastOrbSmall(config){
    def toHexString(width: Int, b: BigInt): String = {
        var s = b.toString(16)
        if (s.length < width) {
            s = "0" * (width - s.length) + s
        }
        s
    }

    def init = {
        clockDomain.forkStimulus(5)

        io.sData.valid #= false
        io.sData.payload #= 0
        io.start #= false
        io.sizeIn.rowNum #= 480 - 1
        io.sizeIn.colNum #= (640 >> 3) - 1
        io.threshold #= 40
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
//            io.mData.ready #= true
            io.mData.ready.randomize()
            if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
                io.start #= false
                val temp = dstFile(iter)
                val colNum = io.mData.payload.size.colNum.toInt
                val rowNum = io.mData.payload.size.rowNum.toInt
                val data = io.mData.payload.score.toInt
                val o = toHexString(4, colNum) + toHexString(4, rowNum) + toHexString(2, data)
//                val o = toHexString(16, io.mData.payload.toBigInt)
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

object TbFast extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbFastOrb1(FastConfig())).doSimUntilVoid { dut =>
        dut.init
        dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\slamData\\fast"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\input.coe")
        dut.out(path + "\\simDataout.coe",path + "\\opencvDataOutFp.coe")
    }
}

case class TbDataGenerateImage() extends dataGenerateImage{
    def toHexString(width: Int, b: BigInt): String = {
        var s = b.toString(16)
        if (s.length < width) {
            s = "0" * (width - s.length) + s
        }
        s
    }

    def init = {
        clockDomain.forkStimulus(5000)
        io.mdata.ready #= true
        clockDomain.waitSampling(10)
    }

    def out(dst_scala: String, dst: String): Unit = {
        clockDomain.waitSampling()
        val testFile = new PrintWriter(new File(dst_scala))
        val dstFile = Source.fromFile(dst).getLines().toArray
        val total = dstFile.length
        var error = 0
        var iter = 0
        var i = 0
        while (i < 640 * 512) {
            clockDomain.waitSampling()
            if (io.mdata.valid.toBoolean && io.mdata.ready.toBoolean) {
                val temp = dstFile(iter)
                val data = io.mdata.payload.toBigInt
                val o = toHexString(8, data)
                i = i + 1
                testFile.write(o + "\r\n")
            }
        }

        clockDomain.waitSampling(10000)
        testFile.close()
        simSuccess()
    }
}

object TbDataGenerateImage extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbDataGenerateImage).doSimUntilVoid { dut =>
        dut.init
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\slamData\\fast"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.out(path + "\\simDataout.coe",path + "\\opencvDataOutFp.coe")
    }
}