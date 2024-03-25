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

//实现nms和nms1，返回，nms的结果和nms1的结果。
object Comparison2{
    //分别计算nms和nms1
    def apply(compareData: Vec[Vec[Bits]], y: Array[Int], x: Array[Int]): Bits = { //中间的数大于所有的数，那么这个点可以被保留
        val isGreater = Vec(Bool(), 8)
        val isGreaterMask = Vec(Bool(), 8)
        val dataWidth = compareData.head.head.getWidth - 1
        for (i <- 0 to 7) {
            isGreater(i) := compareData(y(i))(x(i))(dataWidth downto 0).asUInt < compareData(1)(1)(dataWidth downto 0).asUInt
            isGreaterMask(i) := isGreater(i) || (!compareData(y(i))(x(i))(dataWidth))
        }
        isGreater.andR ## (isGreaterMask.andR && compareData(1)(1).orR)
    }
}
