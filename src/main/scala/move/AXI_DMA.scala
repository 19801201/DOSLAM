package move

import spinal.lib.experimental.chisel.Bundle
import spinal.core.{U, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Aw, Axi4Config, Axi4WriteOnly}
import spinal.lib.bus.amba4.axis.Axi4Stream.{Axi4Stream, Axi4StreamBundle}
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.fsm._


/**
 * @state IDLE 空闲状态可以接收读写描述符，接收描述符后进入开始状态，不接收数据，进入其他状态后开始接收数据
 * @state START 开始状态 传输一个突发传输并进入 WRITE状态
 * @state WRITE 写状态 开始传输数据 一次突发读写
 * @state FINISH_BRUST 提前接收到了last信号，那么进入这个状态，清空这一次的数据
 * @state DROP_DATA 正常完成，清空数据，不在接收
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */
class axiDmaFsm extends StateMachine {//分为四个状态，空闲，有效，和输出down的数据，END
  setEncoding(binaryOneHot)
  val IDLE = new State with EntryPoint
  val START = new State
  val WRITE = new State
  val FINISH_BRUST = new State
  val DROP_DATA = new State

  val start, gotoWrite, finishTr, finishButDrop, finishButBrust, continueTr = Bool()
  val finishEnd,dropEnd = Bool()
  IDLE
  .whenIsActive {
    when(start) {
      goto(START)
    }
  }
  START
    .whenIsActive {
      when(gotoWrite) {
        goto(WRITE)
      }
    }
  WRITE
    .whenIsActive {
      when(finishTr) {
        goto(IDLE)
      } elsewhen(finishButDrop){
        goto(DROP_DATA)
      } elsewhen(finishButBrust){
        goto(FINISH_BRUST)
      } elsewhen(continueTr){
        goto(START)
      }
    }
  FINISH_BRUST
    .whenIsActive {
      when(finishEnd){
        goto(IDLE)
      }
    }
  DROP_DATA
    .whenIsActive{
      when(dropEnd){
        goto(IDLE)
      }
    }
}

//object Axi4_DMA_Write{
//  val page_mask = U"12'hfff"
//  val slave_boundary = U"13'h1000"
//
//  def get_word_4k_boundary(addr:UInt): UInt ={
//    val new_addr = cloneOf(addr)
//    new_addr := slave_boundary - page_mask & addr
//    new_addr
//  }
//}

/*
分别对读口和写口做修改，
  以写口为例，得到的数据，当前数据量大于突发长度才会发一个突发。
  读口当前可用容量大于突发长度，才会发送一个突发。
 */
class dataSave(input:Axi4StreamBundle) extends Bundle(){
  val data = input.data
  val last = input.last
  val strb = input.keep
}

class Axi4_DMA_Write(config : DMA_CONFIG) extends Component {
  val io = new Bundle{
    val m_axi_s2mm = master(Axi4WriteOnly(config.axiConfig))
    val s_axis_s2mm = slave(Axi4Stream(config.axisConfig))
    val s_axis_write_des = slave Stream axis_desc(config)
  }

  val fsm = new axiDmaFsm()
  //组合逻辑
  val wrDesNext = axis_desc(config)
  val trSizeCountNext = UInt(13 bits)
  val aw = Stream(Axi4Aw(config.axiConfig))
//  val inputCycleNext = UInt(8 bits)
  val outputCycleNext = UInt(8 bits)
  val outputLastCycleNext = Bool()
  //时序逻辑
  val wrDesReg = RegNext(wrDesNext)
  val trSizeCountReg = RegNext(trSizeCountNext) init 0
  io.m_axi_s2mm.aw <> aw.m2sPipe()
//  val inputCycleReg = RegNext(inputCycleNext)
  val outputCycleReg = RegNext(outputCycleNext) init 0
  val outputLastCycleReg = RegNext(outputLastCycleNext) init False

  /**
   * 组合逻辑判断
   */

  //代表完成本次传输，但是剩余传输次数不正确
  //满足这个条件的时候，发出控制地址，并且

  /**
   * 地址和发送长度更新策列，idle状态接收传输描述符或者发送控制信息状态没有有效信息占用总线，下一次地址描述符计算公式
   */
  io.s_axis_write_des.ready := fsm.isActive(fsm.IDLE)
  when(fsm.start){
    wrDesNext := io.s_axis_write_des.payload
  } elsewhen(fsm.gotoWrite)(
    wrDesNext := wrDesReg.getNextIncDesc(trSizeCountNext)
  ) otherwise{
    wrDesNext := wrDesReg
  }
  /**
   * 一次突发传输字节数计算，在开始状态时计算传输数据的地址和长度
   * 1、跨越4k边界
   * 2、大于突发传输大小
   * 3、小于突发传输大小和突发容量
   *
   * 计算需要发送的周期数
   */
  when(fsm.isActive(fsm.START)){
    when(wrDesReg.stBurstSize()){//包小于突发长度
      when(wrDesReg.isCrossesBoundary(wrDesReg.len)){//跨越4k地址边界
        trSizeCountNext := wrDesReg.get4kBoundarySize
      } otherwise{//没有跨越4k地址边界
        trSizeCountNext := wrDesReg.len.resized
      }
    }otherwise{//大于4k地址边界
      when(wrDesReg.isCrossesBoundary(config.min4kBrustSize)){//跨越4k地址边界
        trSizeCountNext := wrDesReg.get4kBoundarySize
      } otherwise{//没有跨越4k地址边界
        trSizeCountNext := config.min4kBrustSize
      }
    }
  } otherwise{
    trSizeCountNext := trSizeCountReg
  }
    /**
   * aw通道控制信息,类型固定，地址 突发大小不固定
   */
  aw.payload.setSize(config.maxSingleBrustSize)
  aw.payload.setBurstINCR()
  aw.payload.setCache(B"4'b0011") //写入可使用buff缓存
  aw.payload.setLock(B"1'b0") //不锁定总线
  aw.payload.setProt(B"3'b010")
  aw.payload.len := fsm.gotoWrite.mux(outputCycleNext, io.m_axi_s2mm.aw.len)
  aw.payload.id := U"4'b0"
  //被接收数据后，清空valid有效信号 否则就一直保持，等待
  aw.payload.addr := fsm.gotoWrite.mux(wrDesReg.addr, io.m_axi_s2mm.aw.addr)
  aw.valid := fsm.gotoWrite.mux(True, io.m_axi_s2mm.aw.valid && !io.m_axi_s2mm.aw.ready)

  /**
   * 开始进数据，进行数据传输,传输逻辑判断
   */
  val fifo = StreamFifo(io.s_axis_s2mm.payloadType, config.fifoDepth)
  fifo.io.push <> io.s_axis_s2mm.continueWhen(!fsm.isActive(fsm.IDLE) && !fsm.isActive(fsm.FINISH_BRUST))
//  io.m_axi_s2mm.w.last 重新赋值
  //待传输的数据 包含三层逻辑
  //1、写状态时正常传输
  //2、完成剩余的突发时，传递无效数据
  //3、其余情况不传递数据
  when(fsm.isActive(fsm.WRITE)){
    io.m_axi_s2mm.w.payload.last := outputLastCycleReg & io.m_axi_s2mm.w.valid
    io.m_axi_s2mm.w.payload.data := fifo.io.pop.payload.data
    io.m_axi_s2mm.w.payload.strb := fifo.io.pop.payload.keep
    io.m_axi_s2mm.w.valid := fifo.io.pop.valid
    fifo.io.pop.ready := io.m_axi_s2mm.w.ready
  } elsewhen(fsm.isActive(fsm.FINISH_BRUST)){
    io.m_axi_s2mm.w.payload.last := outputLastCycleReg & io.m_axi_s2mm.w.valid
    io.m_axi_s2mm.w.payload.data := fifo.io.pop.payload.data
    io.m_axi_s2mm.w.payload.strb := B(0, config.strbWidth bits)
    io.m_axi_s2mm.w.valid := True
    fifo.io.pop.ready := False
  } otherwise{
    io.m_axi_s2mm.w.payload.last := False
    io.m_axi_s2mm.w.payload.data := B(0, config.dataWidth bits)
    io.m_axi_s2mm.w.payload.strb := B(0, config.strbWidth bits)
    io.m_axi_s2mm.w.valid := False
    fifo.io.pop.ready := True
  }

  //传输逻辑判断，突发传输开始计算传输次数，每次传输完成一次，记录减少一次传输次数
  when(fsm.gotoWrite){
    outputCycleNext := ((trSizeCountNext - 1) >> config.maxSingleBrustSize).resized
  } elsewhen(io.m_axi_s2mm.w.fire){
    outputCycleNext := outputCycleReg - 1
  } otherwise{
    outputCycleNext := outputCycleReg
  }
  //本次数据传输完成
  outputLastCycleNext := outputCycleNext === U(0).resized

  //状态机的状态跳转条件
  fsm.gotoWrite := fsm.isActive(fsm.START) && !io.m_axi_s2mm.aw.valid
  fsm.start := fsm.isActive(fsm.IDLE) && io.s_axis_write_des.fire
  fsm.finishTr := fifo.io.pop.payload.last && outputLastCycleReg && io.m_axi_s2mm.w.fire
  fsm.finishButBrust := fifo.io.pop.payload.last && !(outputLastCycleReg && io.m_axi_s2mm.w.fire)
  fsm.continueTr := outputLastCycleReg && io.m_axi_s2mm.w.fire && wrDesReg.len > 0
  fsm.finishButDrop := outputLastCycleReg && io.m_axi_s2mm.w.fire && wrDesReg.len === 0

  fsm.finishEnd := outputLastCycleReg && io.m_axi_s2mm.w.fire
  fsm.dropEnd := fifo.io.pop.payload.last

//  when(fifo.io.pop.payload.last){//接收到最后的数据
//    when(outputLastCycleReg && io.m_axi_s2mm.w.fire){//传输最后一个数据
//      fsm.finishTr := False
//    } otherwise{
//      fsm.finishButBrust := False
//    }
//  }
//  when(outputLastCycleReg && io.m_axi_s2mm.w.fire){
//    when(wrDesReg.len > 0){
//      fsm.continueTr := True
//    }elsewhen{
//      fsm.finishButDrop := True
//    }
//  }

  io.m_axi_s2mm.b.ready := True
}

object Axi4_DMA_Write extends App {
  SpinalVerilog(new Axi4_DMA_Write(DMA_CONFIG()))
}
