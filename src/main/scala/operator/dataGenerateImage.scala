package operator

package operator
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import utils._

import wa.WaCounter

class dataGenerateImage(col:Int = 640, row:Int = 512) extends Module{
    val io = new Bundle{
        val mdata = master Stream(UInt(32 bits))
        val last, user = out Bool()
        val keep = out Bits(4 bits)
    }
    //首先行计数 + 1
    val vvalid = Reg(Bool()) init(False)
    val hvalid = Reg(Bool()) init(False)
    for(i <- 0 until  4) io.keep(i) := io.mdata.valid
    val cnt = ImageCount(io.mdata.fire,new ImageSize(16, U(col - 1, 16 bits), U(row - 1, 16 bits)))
    val hCnt = WaCounter(io.mdata.fire, 16, U(col - 1, 16 bits))

    val readyCnt = WaCounter(!io.mdata.valid, 16, 1024)
    io.mdata.payload := (cnt.rowCnt.count) @@ (cnt.colCnt.count)
    io.user := io.mdata.valid && !cnt.rowCnt.count.orR && !cnt.colCnt.count.orR
    io.last := (io.mdata.valid && cnt.colCnt.valid) || (!io.mdata.valid)
    vvalid.setWhen(readyCnt.valid) clearWhen(cnt.fireCnt)
    hvalid.setWhen(readyCnt.valid) clearWhen(hCnt.valid && io.mdata.fire)
    io.mdata.valid := vvalid && hvalid
}

object dataGenerateImage extends App {
    SpinalVerilog(new dataGenerateImage())
}

