package TbOperator

import operator._
import spinal.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Random
case class TbNMS1(config : NMSConfig) extends NMS1(config){
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
        clockDomain.waitSampling(10)
        io.sizeIn.rowNum #= 640 - 1
        io.sizeIn.colNum #= (640 >> 3) - 1
        io.mData.ready #= false
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
            io.mData.ready.randomize()
            if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
                //io.start #= false
                val temp = dstFile(iter)
                val colNum = io.mData.payload.size.colNum.toInt
                val rowNum = io.mData.payload.size.rowNum.toInt
                val data = io.mData.payload.score.toInt
                val t1 = toHexString(2,colNum % 256)
                val t2 = toHexString(2,colNum / 256)
                val t3 = toHexString(2,rowNum % 256)
                val t4 = toHexString(2,rowNum / 256)
                val t5 = toHexString(2,data)
                val o = t5 + t2 + t1 + t4 + t3

                if (!temp.equals(o)) {
                    error = error + 1
                    printf(i + "\n");
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

case class TbNMS(config : NMSConfig) extends NMS(config){
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
        clockDomain.waitSampling(10)
        io.rowNumIn #= 640
        io.colNumIn #= 640 >> 3
        io.mData.ready #= true
    }

    def in(src: String): Unit = {
        val random = new Random()
        fork {
            for (line <- Source.fromFile(src).getLines) {
                io.sData.payload #= BigInt(line.trim, 16)
                io.sData.valid #= true
                clockDomain.waitSamplingWhere(io.sData.ready.toBoolean)
                //                io.sData.valid #= false
                //                val randomInt = random.nextInt(25)
                //                if (randomInt < 4) clockDomain.waitSampling(randomInt)
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
            if (io.mData.valid.toBoolean) {
                //io.start #= false
                val temp = dstFile(iter)
                val colNum = io.mData.payload.colNum.toInt * 8 + io.mData.payload.selNum.toInt
                val rowNum = io.mData.payload.rowNum.toInt
                val data = io.mData.payload.dataPoint.toInt
                val t1 = toHexString(2,colNum % 256)
                val t2 = toHexString(2,colNum / 256)
                val t3 = toHexString(2,rowNum % 256)
                val t4 = toHexString(2,rowNum / 256)
                val t5 = toHexString(2,data)
                val o = t5 + t2 + t1 + t4 + t3

                if (!temp.equals(o)) {
                    error = error + 1
                    printf(i + "\n");
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


object TbNMS extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbNMS(NMSConfig())).doSimUntilVoid { dut =>
        dut.init
        dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\OpencvData\\NMS"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}

object TbNMS1 extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbNMS1(NMSConfig())).doSimUntilVoid { dut =>
        dut.init
        dut.clockDomain.waitSampling(10)
        dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\OpencvData\\NMS"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}