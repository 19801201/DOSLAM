package TbData
import data.{ReflectionFillWindow, ReflectionFillWindowConfig, Windows, WindowsConfig}
import spinal.core._
import spinal.core.sim._
import java.io.{File, PrintWriter}
import scala.io.Source
case class TbReflectionFillWindow(config : ReflectionFillWindowConfig) extends ReflectionFillWindow(config){
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
        io.mReady #= false
        io.start #= false
        //        io.enPadding(0) #= true
        //        io.enPadding(1) #= true
        //        io.enPadding(2) #= false
        //        io.enPadding(3) #= true
        io.rowNumIn #= 640
        io.colNumIn #= 640 >> 3
        clockDomain.waitSampling(10)
    }

    def in(src: String): Unit = {
        fork {
            for (line <- Source.fromFile(src).getLines) {
                io.sData.payload #= BigInt(line.trim, 16)
                io.sData.valid #= true
                clockDomain.waitSamplingWhere(io.sData.ready.toBoolean)
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
            io.mReady #= true //不使用反压功能
            //io.mData.ready.randomize() //使用反压功能
            if (io.mData.valid.toBoolean) {
                io.start #= false
                for (h <- 0 until config.WINDOWS_SIZE_H) {
                        val temp = dstFile(iter)
                        val o = toHexString(16, io.mData.payload(h).toBigInt)

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


object TbReflectionFillWindow extends App {
    val spinalConfig = new SpinalConfig(
        defaultClockDomainFrequency = FixedFrequency(200 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH, resetKind = SYNC)
    )
    //SimConfig.withXSim.withWave.withConfig(spinalConfig).compile(new TbMaxPooling()).doSimUntilVoid { dut =>
    SimConfig.withWave.withConfig(spinalConfig).compile(new TbReflectionFillWindow(ReflectionFillWindowConfig())).doSimUntilVoid { dut =>
        dut.init
        dut.io.start #= true
        val path = "F:\\TestData\\OpencvData\\ReflectionFillWindow"
        //dut.in("G:\\SpinalHDL_CNN_Accelerator\\simData\\paddingSrc.txt")
        dut.in(path + "\\ReferenceDataIn.txt")
        dut.out(path + "\\dstDataOut.txt",path + "\\ReferenceDataOut.txt")
    }
}