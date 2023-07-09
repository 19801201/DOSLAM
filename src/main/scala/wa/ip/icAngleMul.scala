package wa.ip

import org.apache.commons.io.FileUtils
import spinal.core._
import config.Config.filePath

class icAngleMul(clockDomain: ClockDomain) extends BlackBox {
    val io = new Bundle {
        val CLK = in Bool()
        val SEL = in Bool()
        val A = in SInt(14 bits)
        val D = in SInt(14 bits)
        val B = in SInt(5 bits)
        val P = out SInt(48 bits)
    }
    noIoPrefix()
    mapClockDomain(clockDomain, io.CLK)
}
