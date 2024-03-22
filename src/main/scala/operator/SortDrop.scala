package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

import scala.math._

class SortDrop[T <: Data](dataType: HardType[T], len: Int, compare: (T, T) => Bool) extends Module{
  val io = new Bundle{
    val sData = slave(Stream(dataType))
    val done = in Bool()//完成信号，
    val mData = master(Stream(dataType))
    val outputLast = out Bool()
    val inputLength = out(UInt(16 bits))
    val outputLength = out(UInt(16 bits))

    val flush = in Bool() //刷新寄存器

    val topNum = in(UInt(16 bits))
    val defaultData = in(dataType)
  }
  //对这些数据进行排序
  val sort = new MergeSortT(dataType, len, compare)
  val merge = new TopMerge(dataType, len, compare)

  val inputCnt = WaCounter(io.sData.fire, 16, (1 << 16) - 1) //记录输入数据个数
  val outputCnt = WaCounter(io.mData.fire, 16, (1 << 16) - 1) //记录输出数据个数
  val midCnt = WaCounter(sort.io.ido.fire, 16, (1 << 16) - 1) //记录中间的结果个数
  //填充不足的len的大小
  val paddingCnt = WaCounter(sort.io.idi.fire, 16, len - 1)

  //结束信号
  val done = RegInit(False) setWhen(io.done) clearWhen(io.flush)
  //填充数据，将结果输出
  val topPaddingValid = RegInit(False) setWhen (done && !paddingCnt.count.orR) clearWhen(io.flush)


  //数据输入，填充数据
  when(!done){
    io.sData <> sort.io.idi
  }otherwise{
    sort.io.idi.valid := (paddingCnt.count.orR || !topPaddingValid) //填充一行多数据
    sort.io.idi.payload := io.defaultData
    io.sData.ready := False
  }
  //数据进入TopMerge中
  sort.io.ido <> merge.io.input
  //最后的使能信号判断

  val enLast = RegInit(False) setWhen(done && inputCnt.count === midCnt.count) clearWhen(io.flush)
  merge.io.enLastVec := enLast
  //为merge填充数据
  io.inputLength := inputCnt.count
  io.outputLength := outputCnt.count

  merge.io.output.throwWhen(outputCnt.count > io.topNum || outputCnt.count >= inputCnt.count) <> io.mData

  io.outputLast := done && (outputCnt.count + 1 === io.topNum || outputCnt.count + 1 === inputCnt.count)

  when(io.flush){
    inputCnt.clear
    outputCnt.clear
    midCnt.clear
    paddingCnt.clear
  }

}


object SortDrop extends App {
  SpinalVerilog(new SortDrop(SInt(32 bits), 64, (left: SInt, right: SInt) => left < right))
}