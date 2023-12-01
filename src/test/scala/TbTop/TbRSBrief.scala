package TbTop

import operator.{GaussianBlur, GaussianBlurConfig}
import spinal.core._
import spinal.core.sim._

import java.io.{File, PrintWriter}
import scala.io.Source
import top._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class TbRSBrief(config : RSBriefConfig) extends RSBriefOrb(config){
    def toHexString(width: Int, b: BigInt): String = {
        var s = b.toString(16)
        if (s.length < width) {
            s = "0" * (width - s.length) + s
        }
        s
    }

    def init = {
        clockDomain.forkStimulus(5000)

        io.sDataImage.valid #= false
        io.sDataImage.payload #= 0

        io.sDataFeaturePoint.valid #= false
        io.sDataFeaturePoint.payload.score #= 0
        io.sDataFeaturePoint.payload.size.colNum #= 0
        io.sDataFeaturePoint.payload.size.rowNum #= 0
        io.start #= false
        io.sizeIn.rowNum #= 480 - 1
        io.sizeIn.colNum #= (640 >> 3) - 1
        io.mDataRsBrief.ready #= false
        clockDomain.waitSampling(10)
    }

    def in(srcImage: String, srcFp:String): Unit = {
        val random = new Random()
        fork {
            for (line <- Source.fromFile(srcImage).getLines) {
                io.sDataImage.payload #= BigInt(line.trim, 16)
                io.sDataImage.valid #= true
                clockDomain.waitSamplingWhere(io.sDataImage.ready.toBoolean)
//                val randomInt = random.nextInt(25)
//                if (randomInt < 4) clockDomain.waitSampling(randomInt)
            }
            io.sDataImage.valid #= false
        }

        fork{
            for (line <- Source.fromFile(srcFp).getLines) {
                io.sDataFeaturePoint.payload.size.colNum #= BigInt(line.trim.substring(0, 4), 16)
                io.sDataFeaturePoint.payload.size.rowNum #= BigInt(line.trim.substring(4, 8), 16)
                io.sDataFeaturePoint.payload.score #= BigInt(line.trim.substring(8, 10), 16)
                io.sDataFeaturePoint.valid #= true
                clockDomain.waitSamplingWhere(io.sDataFeaturePoint.ready.toBoolean)
            }
            io.sDataFeaturePoint.valid #= false
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
            io.mDataRsBrief.ready #= true
//            io.mDataRsBrief.ready.randomize()
            if (io.mDataRsBrief.valid.toBoolean && io.mDataRsBrief.ready.toBoolean) {
                io.start #= false
                val temp = dstFile(iter)
                val o = toHexString(16, io.mDataRsBrief.payload.toBigInt)
                println(o)
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

object TbRSBrief extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.withConfig(spinalConfig).compile(new TbRSBrief(RSBriefConfig())).doSimUntilVoid { dut =>
        dut.init
        dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\slamData\\RSBrief"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\ReferenceDataInImage.coe", path + "\\ReferenceDataInKeypoints.coe")
        dut.out(path + "\\simDataout.coe",path + "\\ReferenceDataOutputDescriptors.coe")
    }
}