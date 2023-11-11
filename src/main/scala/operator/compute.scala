package operator
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object Comparison{
    def apply(compareData: Vec[Vec[Bits]], y: Array[Int], x: Array[Int]): Bool = { //中间的数大于所有的数，那么这个点可以被保留
        val isGreater = Vec(Bool(), 8)
        for (i <- 0 to 7) {
            isGreater(i) := compareData(y(i))(x(i)).asUInt < compareData(1)(1).asUInt
        }
        isGreater.andR
    }
}
