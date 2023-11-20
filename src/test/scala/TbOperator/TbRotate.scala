package TbOperator

import operator._
import spinal.core._
import spinal.core.sim._

import scala.util.Random
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class TbRotate(config : rotateConfig) extends Rotate(config){
    def toHexString(width: Int, b: BigInt): String = {
        var s = b.toString(16)
        if (s.length < width) {
            s = "0" * (width - s.length) + s
        }
        s
    }

    def init = {
        clockDomain.forkStimulus(5000)

        io.sDataTan.valid #= false
        io.sDataBrief.valid #= false
        //io.start #= false
        //io.enPadding(0) #= true
        //io.enPadding(1) #= true
        //io.enPadding(2) #= false
        //io.enPadding(3) #= true
        //io.rowNumIn #= 640
        //io.colNumIn #= 640 >> 3
        //io.inValid #= 7
        clockDomain.waitSampling(10)
    }

    def in(src: String): Unit = {
//        fork {
//            var count = 0
//            var sum = 0
//            for (line <- Source.fromFile(src).getLines) {
//                io.sDataBrief.payload #= BigInt(line.trim, 16)
//                io.sDataBrief.valid #= true
//                count = count + 1
//                if (count == 8) {
//                    count = 0;
//                    io.sDataTan.payload #= sum / 100
//                    io.sDataTan.valid #= true
//                    sum = sum + 1
//                }
//                clockDomain.waitSampling()
//                io.sDataTan.valid #= false
//                io.sDataBrief.valid #= false
//            }
//        }
        fork {
            var count = 0
            var sum = 0
            val random = new Random()
            for (line <- Source.fromFile(src).getLines) {
                io.sDataBrief.payload #= BigInt(line.trim, 16)
                io.sDataBrief.valid #= true
                count = count + 1
                if (count == 8) {
                    count = 0;
                    clockDomain.waitSampling()
                    io.sDataBrief.valid #= false
                    val stop = random.nextInt(8)
                    clockDomain.waitSampling(stop)
                    sum = sum + 1
                }
                clockDomain.waitSampling()
                io.sDataBrief.valid #= false
            }
        }
    }

    def in2(n:Int): Unit = {
        fork {
            val random = new Random() //延迟若干个周期后给下一个数
            for(num <- 0 until 32 * 100){
                val stop = random.nextInt(n)
                io.sDataTan.valid #= false
                clockDomain.waitSampling(stop)
                while(fifoTan.io.availability.toInt <= 1){
                    clockDomain.waitSampling(8)
                }
                io.sDataTan.payload #= num / 100
                io.sDataTan.valid #= true
                clockDomain.waitSampling()
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
            io.mDataRsBrief.ready #= true
            if (io.mDataRsBrief.ready.toBoolean && io.mDataRsBrief.valid.toBoolean) {//如果接收到数据
                val temp = dstFile(iter)
                val o = toHexString(16, io.mDataRsBrief.payload.toBigInt)
                //println(io.mData.payload(0).toInt)
                //println(io.mData.payload(1).toInt)
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


object TbRotate extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbRotate(rotateConfig())).doSimUntilVoid { dut =>
        dut.init
        //dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\OpencvData\\rotate"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in2(16)
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}