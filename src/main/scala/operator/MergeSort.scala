package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

import scala.math._

/*
    开始信号发出之后，首先刷新数据
 */
case class MergeSortConfig(maxNum : Int = 640){
  //val maxUseReg = 64
}

trait VecIoPara{
  val ControlWidth = 12
  val DataWidth = 32

  val ControlWB = ControlWidth bits
  val DataWB = DataWidth bits
}
//固定的
case class VecControlIO() extends Bundle with IMasterSlave with VecIoPara{
  val start    = Bool()
  val last     = Bool()
  //start代表开始信号，last代表结束信号，数据已经发生完成
  override def asMaster(): Unit = {
    out(start, last)
  }
}

//case class VecDataIO() extends Bundle with IMasterSlave with VecIoPara{
//    val data =
//
//    override def asMaster(): Unit = {
//        out(data)
//    }
//}

class Merge[T <: Data](dataType: HardType[T], len: Int, compare: (T, T) => Bool) extends Module {
  val io = new Bundle {
    val input = slave(Stream(dataType))
    val output = master(Stream(dataType))
  }
  val widthBits = max(log2Up(len), 2)

  def isPowerOfTwo(x: Int): Boolean = {
    if (x <= 0) {
      false
    } else {
      (x & (x - 1)) == 0
    }
  }

  assert(isPowerOfTwo(len))
  println(len + ":" + max(log2Up(len), 1))
  val switchCount = WaCounter(io.input.fire, widthBits, len - 1)
  val UseA = Reg(Bool()) init (True) toggleWhen (switchCount.valid && io.input.fire)
  val fifoA = StreamFifo(io.input.payload, len + (if (len < 64) 1 else 0))
  val fifoB = StreamFifo(io.input.payload, len)

  val selA = compare(fifoA.io.pop.payload, fifoB.io.pop.payload)
  io.output.payload := RegNextWhen(fifoA.io.pop.fire.mux(fifoA.io.pop.payload, fifoB.io.pop.payload), fifoA.io.pop.fire || fifoB.io.pop.fire)
  io.output.valid := Reg(Bool(), False) clearWhen io.output.fire setWhen fifoA.io.pop.fire || fifoB.io.pop.fire

  fifoA.io.push.payload := io.input.payload
  fifoB.io.push.payload := io.input.payload
  io.input.ready := (fifoA.io.push.ready && UseA) || (fifoB.io.push.ready && !UseA)
  fifoA.io.push.valid := UseA && io.input.valid
  fifoB.io.push.valid := !UseA && io.input.valid

  val Acount = WaCounter(fifoA.io.pop.fire, widthBits, len - 1)
  val Bcount = WaCounter(fifoB.io.pop.fire, widthBits, len - 1)
  val state = Reg(UInt(2 bits), U"2'b00") //比较状态
  val isCompareState = (state === U"2'b00")
  val isAoutput = (state === U"2'b01")
  val isBoutput = (state === U"2'b10")
  when(Acount.valid && fifoA.io.pop.fire) {
    state := isCompareState.mux(U"2'b10", U"2'b00")
  }
  when(Bcount.valid && fifoB.io.pop.fire) {
    state := isCompareState.mux(U"2'b01", U"2'b00")
  }
  fifoA.io.pop.ready := (io.output.ready || !io.output.valid) && ((isCompareState && selA && fifoB.io.pop.valid && fifoA.io.pop.valid) || isAoutput)
  fifoB.io.pop.ready := (io.output.ready || !io.output.valid) && ((isCompareState && !selA && fifoB.io.pop.valid && fifoA.io.pop.valid) || isBoutput)
}

class TopMerge[T <: Data](dataType: HardType[T], len: Int, compare: (T, T) => Bool) extends Module{
  val io = new Bundle {
    val input = slave(Stream(dataType))
    val output = master(Stream(dataType))
    val enLastVec = in Bool()
  }
  val widthBits = max(log2Up(len), 2)

  def isPowerOfTwo(x: Int): Boolean = {
    if (x <= 0) {
      false
    } else {
      (x & (x - 1)) == 0
    }
  }

  assert(isPowerOfTwo(len))
  //println(len + ":" + max(log2Up(len), 1))
  val fsm = new StateMachine {
    setEncoding(binaryOneHot)
    val INIT = new State with EntryPoint
    val INPUTC, OUTPUT, WAIT = new State
    val inputCnt = WaCounter(io.input.fire, log2Up(len), len - 1)
    val outputCnt = WaCounter(io.output.fire, log2Up(len), len - 1)
    val bclear = Reg(Bool(), False) setWhen outputCnt.valid && io.output.fire clearWhen isActive(INIT)
    val bclear2 = Bool()
    val clear = RegNext((((outputCnt.valid && io.output.fire) || bclear) && bclear2) && isActive(OUTPUT)) init False

    INIT
      .whenIsActive {
        when(inputCnt.valid && io.input.fire){
          when(io.enLastVec){
            goto(OUTPUT)
          } otherwise{
            goto(INPUTC)
          }
        }
      }
    INPUTC
      .whenIsActive {
        when(inputCnt.valid && io.input.fire) {
          when(io.enLastVec) {
            goto(OUTPUT)
          }
        }
      }
    OUTPUT
      .whenIsActive {
        when(((outputCnt.valid && io.output.fire) || bclear) && bclear2) {
          goto(INIT)
        }
      }
  }
  //开始输入的先给fifoA，完成侯输入给B
  val output = weakCloneOf(io.output)

  val switchCount = WaCounter(output.fire, widthBits, len - 1)
  val inputDataUseA = Reg(Bool()) init (False) toggleWhen (switchCount.valid && output.fire)
  val fifoA = StreamFifo(io.input.payload, len)//初始输入缓存
  val fifoB = StreamFifo(io.input.payload, len/2)//暂存数据缓存
  val fifoC = StreamFifo(io.input.payload, len)//结果输出缓存

  val clearFifoB = Reg(Bool(), False)

  //fifo输出比较计数
  val anyFifoPopValid = fifoA.io.pop.fire || fifoB.io.pop.fire || fifoC.io.pop.fire
  val selCompareCount = WaCounter(fifoA.io.pop.fire || fifoC.io.pop.fire, widthBits, len - 1)
  val fifoBComparecount = WaCounter(fifoB.io.pop.fire, widthBits, len - 1)
  val curEndCount = WaCounter(anyFifoPopValid && !clearFifoB, widthBits, len - 1)

  fsm.bclear2 := fifoBComparecount.valid && fifoB.io.pop.fire
  clearFifoB setWhen curEndCount.valid && anyFifoPopValid clearWhen fifoBComparecount.valid && fifoB.io.pop.fire
  val clearFifoBRise = (curEndCount.valid && anyFifoPopValid).rise()
  //输入切换 input
  //输出切换

  val selCompareA = RegInit(True) toggleWhen((selCompareCount.valid && (fifoA.io.pop.fire || fifoC.io.pop.fire)) || (curEndCount.valid && anyFifoPopValid || fsm.clear))
  val selCompareData = selCompareA.mux(fifoA.io.pop.payload, fifoC.io.pop.payload)

  val selB = compare(selCompareData, fifoB.io.pop.payload)

  val judgeEnd = RegInit(False) setWhen !curEndCount.count.orR clearWhen(fsm.clear)
  val judgeLastEnd = judgeEnd || !curEndCount.count.orR

  output.payload := RegNextWhen(fifoA.io.pop.fire.mux(fifoA.io.pop.payload,fifoB.io.pop.fire.mux(fifoB.io.pop.payload, fifoC.io.pop.payload)), anyFifoPopValid && !clearFifoB)
  output.valid := Reg(Bool(), False) clearWhen output.fire setWhen anyFifoPopValid && !clearFifoB
  output.ready := !output.valid || (judgeLastEnd.mux(io.output.ready, True))
  io.output.payload := output.payload
  io.output.valid := output.valid && fsm.isActive(fsm.OUTPUT)

  fifoC.io.push.payload := output.payload
  fifoC.io.push.valid := (output.fire && !fsm.isActive(fsm.OUTPUT)) && !fsm.isActive(fsm.INIT) && !inputDataUseA
  fifoA.io.push.payload := fsm.isActive(fsm.INIT).mux(io.input.payload, output.payload)
  fifoB.io.push.payload := io.input.payload
  fifoA.io.push.valid := (io.input.valid && fsm.isActive(fsm.INIT)) || ((output.fire && !fsm.isActive(fsm.OUTPUT)) && !fsm.isActive(fsm.INIT) && inputDataUseA)
  fifoB.io.push.valid := (io.input.valid && !fsm.isActive(fsm.INIT))
  io.input.ready := fsm.isActive(fsm.INIT).mux(fifoA.io.push.ready, fifoB.io.push.ready)

  //接收完数据，清空fifo 输出设置
  fifoA.io.flush := (selCompareA && clearFifoBRise) || fsm.clear
  fifoC.io.flush := (!selCompareA && clearFifoBRise) || fsm.clear
  fifoB.io.flush := fsm.clear
  when(curEndCount.valid && anyFifoPopValid || fsm.clear){
    selCompareCount.clear
  }
  //    when(fifoBComparecount.valid && fifoB.io.pop.fire || fsm.clear){
  //        curEndCount.clear
  //    }
  when(fsm.clear){
    fifoBComparecount.clear
  }

  //当已经输出output个文件但是，所有的数据还没有，那么需要输出这个数据，top数据可以直接clear，inputComparecount需要把所有的数据清空，这个速度可能会导致整体的流水线减慢一半，
  fifoA.io.pop.ready := (output.ready || !output.valid) && ((selCompareA && !selB && fifoB.io.pop.valid && fifoA.io.pop.valid) && !clearFifoB)
  fifoB.io.pop.ready := (output.ready || !output.valid) && ((selB && fifoB.io.pop.valid && selCompareA.mux(fifoA.io.pop.valid, fifoC.io.pop.valid)) || clearFifoB)
  fifoC.io.pop.ready := (output.ready || !output.valid) && ((!selCompareA && !selB && fifoB.io.pop.valid && fifoC.io.pop.valid) && !clearFifoB)
  //输出状态
}

class MergeSort(config : MergeSortConfig) extends Module with VecIoPara{
  val io = new Bundle {//
    //val ic = slave(VecControlIO())
    val idi = slave Stream SInt(DataWB)
    val ido = master Stream SInt(DataWB)
  }

  val data = Vec(weakCloneOf(io.idi),log2Up(config.maxNum) + 1)
  data.head << io.idi
  io.ido << data.last

  for(i <- 0 until log2Up(config.maxNum)){
    val mergeSize = 1 << i
    val merge =  new Merge(io.idi.payload, mergeSize, (left: SInt, right: SInt) => left < right)
    merge.io.input << data(i)
    data(i + 1) << merge.io.output
  }
}

class MergeA extends Module{
  val io = new Bundle {
    val input = slave(Stream(SInt(32 bits)))
    val output = master(Stream(SInt(32 bits)))
    val enLastVec = in Bool()
  }
  val merge = new TopMerge(SInt(32 bits), 64, (left: SInt, right: SInt) => left < right)
  io.input >> merge.io.input
  io.output << merge.io.output
  merge.io.enLastVec := io.enLastVec
}

object MergeSort {
  def main(args: Array[String]): Unit = {
    SpinalConfig(removePruned = true).generateVerilog(new MergeSort(MergeSortConfig(1024)))
    SpinalConfig(removePruned = true).generateVerilog(new TopMerge(SInt(32 bits), 1024, (left: SInt, right: SInt) => left < right))
    //        val levelHeap = log2Up(65)
    //        for(i <- 0 until log2Up(64)){
    //            println(1 << i)
    //        }
  }
}