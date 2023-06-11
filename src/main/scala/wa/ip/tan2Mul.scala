package wa.ip

import org.apache.commons.io.FileUtils
import spinal.core._
import config.Config.filePath

class tan2Mul(clockDomain: ClockDomain) extends BlackBox {
    val io = new Bundle {
        val CLK = in Bool()
        val SEL = in Bits(2 bits)
        val A = in SInt(20 bits)
        val B = in SInt(18 bits)
        val C = in SInt(38 bits)
        val D = in SInt(25 bits)
        val P = out SInt(44 bits)
    }
    noIoPrefix()
    mapClockDomain(clockDomain, io.CLK)
}
