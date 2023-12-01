package demo

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
class mux extends Component{
    val io = new Bundle{
        val sel = in Bits(4 bits)
        val input = in Vec(Bits(2 bits), 4)
        val output = out Bits(2 bits)
    }
    val io.output = MuxOH(io.sel, io.input)
    //OHToUInt(sel : Seq[Bool])
}
object mux extends App {
    SpinalVerilog(new mux())
}