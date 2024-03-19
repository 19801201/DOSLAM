package TbData

import data.{Windows, WindowsConfig, syncWindows, syncWindowsPadding, syncWindowsPadding2}
import spinal.core._
import spinal.core.sim._

import scala.util.Random
import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
//测试通过 2 1 下一次测试，每五行删除一行数据
class TbSyncWindows(config : WindowsConfig) extends syncWindowsPadding2 (config) {
    val random = new Random()
    val ValidRandomTest = true
//    fsm.validCnt.colCnt.count.simPublic()
//    fsm.validCnt.rowCnt.count.simPublic()
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
        io.mData.ready #= false
        io.start #= false

        io.sizeIn.rowNum #= 120 - 1
        io.sizeIn.colNum #= (160 / 8) - 1
        clockDomain.waitSampling(10)
    }

    def in(src: String): Unit = {
        fork {
            for (line <- Source.fromFile(src).getLines) {//产生随机的拉低数据
                io.sData.payload #= BigInt(line.trim, 16)
                io.sData.valid #= true
                clockDomain.waitSamplingWhere(io.sData.ready.toBoolean)
                if(ValidRandomTest){
                    io.sData.valid #= false
                    var randomNumber = random.nextInt(100)
                    if (randomNumber > 5) {
                        randomNumber = 0
                    }
                    clockDomain.waitSampling(randomNumber)
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
            //io.mData.ready #= true //不使用反压功能
            io.mData.ready.randomize() //使用反压功能
            if (io.mData.valid.toBoolean && io.mData.ready.toBoolean) {
                //接收整个窗口的数据并进行校验
                i = i + config.WINDOWS_SIZE_H * config.WINDOWS_SIZE_W
                io.start #= false
                for(h <- 0 until config.WINDOWS_SIZE_H){
                    for (w <- 0 until config.WINDOWS_SIZE_W) {
                        val temp = dstFile(iter)
                        val o = toHexString(2 * config.DATA_NUM, io.mData.payload(h)(w).toBigInt)
                        if (!temp.equals(o)) {
                            error = error + 1
                            printf(i + "\n");
//                            printf("row:%d, col:%d\n", fsm.validCnt.rowCnt.count.toInt, fsm.validCnt.colCnt.count.toInt)
                            printf("h:%d, w:%d\n", h, w)
//                            if(error > 20){
//                                i+=999999
//                            }
                            //return ;
                        }
                        if (iter % 1000 == 999) {
                            val errorP = error * 100.0 / total
                            println(s"total iter = $total current iter =  $iter :::  error count = $error error percentage = $errorP%")
                        }
                        testFile.write(o + "\r\n")
                        iter = iter + 1
                    }
                }
            }
        }
        if(error>0){
            println(s"error is $error\n")
        } else{
            println(s"ac\n")
        }

        sleep(100)
        testFile.close()
        simSuccess()
    }
}

object TbSyncWindows extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    val dataGenerateRow31Config = WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 7, WINDOWS_SIZE_W = 3,
        MEM_DEPTH = 1024)

    val dataGenerateRow33Config = WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 3, WINDOWS_SIZE_W = 3, MEM_DEPTH = 1024)

    SimConfig.withXilinxDevice("xq7vx690trf1157-2I").withXSimSourcesPaths(ArrayBuffer("src/test/ip"), ArrayBuffer("")).withWave.withXSim.compile(new TbSyncWindows(dataGenerateRow33Config)).doSimUntilVoid { dut =>
        dut.init
        dut.io.start #= true
        val path = "C:\\myData\\data\\xsim_data\\slam\\ReflectionFillWindow3_3"
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}