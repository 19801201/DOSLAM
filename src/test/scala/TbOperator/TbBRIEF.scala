package TbOperator

import operator._
import spinal.core._
import spinal.core.sim._

import scala.util.Random
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
case class TbBRIEF(config : BRIEFConfig) extends BRIEF(config){
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
        io.sData.payload.foreach(_.foreach(_#=0))
        //io.start #= false
        //        io.enPadding(0) #= true
        //        io.enPadding(1) #= true
        //        io.enPadding(2) #= false
        //        io.enPadding(3) #= true
        //io.rowNumIn #= 640
        //io.colNumIn #= 640 >> 3
        //io.inValid #= 7
        clockDomain.waitSampling(10)
    }

    def in(src: String): Unit = {
        fork {
            var count = 0;
            for (line <- Source.fromFile(src).getLines) {
                for (i <- 0 until config.DATA_NUM) {//注意顺序
                    io.sData.payload(count % config.DATA_NUM)(config.DATA_NUM - i - 1) #= BigInt(line.trim.substring(i * 2, i * 2 + 2), 16)
                }
                count = count + 1
                if (count % config.DATA_NUM == 0) {
                    io.sData.valid #= true
                    clockDomain.waitSampling(8)
                    io.sData.valid #= false

                    val random = new Random()//延迟若干个周期后给下一个数
                    val num = random.nextInt(11)

                    clockDomain.waitSampling(num)
                }
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
            if (io.mData.valid.toBoolean) {//valid有效那么说明结果是正确的，开始接收数据
                //io.start #= false
                val temp = dstFile(iter)
                val o = toHexString(8, io.mData.payload.toBigInt)
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


object TbBRIEF extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbBRIEF(BRIEFConfig())).doSimUntilVoid { dut =>
        dut.init
        //dut.io.start #= true
        dut.clockDomain.waitSampling(10)
        val path = "F:\\TestData\\OpencvData\\BRIEF"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}