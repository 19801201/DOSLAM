package move

import spinal.lib.experimental.chisel.Bundle
import spinal.core.{U, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Aw, Axi4Config, Axi4ReadOnly, Axi4WriteOnly}
import spinal.lib.bus.amba4.axis.Axi4Stream.{Axi4Stream, Axi4StreamBundle}
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.fsm._
import wa.WaCounter
import spinal._

class axiDmaReadFsm extends StateMachine {//分为四个状态，空闲，有效，和输出down的数据，END
  setEncoding(binaryOneHot)
  val IDLE = new State with EntryPoint //空闲状态
  val PARA = new State //准备发出搬运信号
  val MOVE = new State //得到数据进行搬运
  val END = new State //等待fifo数据全部派出

  val startPara, startMove, startEnd, popEnd = Bool()
  IDLE
    .whenIsActive {
      when(startPara) {
        goto(PARA)
      }
    }
  PARA
    .whenIsActive {
      when(startMove) {
        goto(MOVE)
      }
    }
  MOVE
    .whenIsActive {
      when(startEnd){
        goto(END)
      } otherwise (startPara){
        goto(PARA)
      }
    }
  END
    .whenIsActive {
      when(popEnd){
        goto(IDLE)
      }
    }
}

class xg_axi_dma_read(config:DMA_CONFIG) extends Module{
  val io = new Bundle{
    val s_axi_mm2s = master(Axi4ReadOnly(config.axiConfig))
    val m_axis_mm2s = master(Axi4Stream(config.axisConfig))
    val s_axis_read_des = slave Stream axis_desc(config)
  }
  val fsm = new axiDmaReadFsm()
  val fifo = StreamFifo(io.m_axis_mm2s.payloadType, config.fifoDepth)

  //传输的起始地址和剩余传输的数据量的描述
  val trDesc = axis_desc(config) setAsReg()
  //判断每次需要传输的数据量和时钟数
  val trLenSize = computeTrSize(trDesc, config)//len传输的字节数量
  val trLenCycle = ((trLenSize - 1) >> config.maxSingleBrustSize).resize(8)//传输的周期数 - 1
  //修改传输的起始地址和剩余字节长度
  when(io.s_axis_read_des.fire){
    trDesc := io.s_axis_read_des.payload
  } elsewhen(fsm.isExiting(fsm.PARA)){
    trDesc.getNextIncDesc(trLenSize)
  }
  //当前的剩余周期数目
  val trCycle = Reg(UInt(8 bits))
  when(fsm.isExiting(fsm.PARA)){
    trCycle := trLenCycle
  } elsewhen (io.s_axi_mm2s.r.fire){
    trCycle := trCycle - 1
  }
  //判断能否进行传输
  fsm.startPara := (RegNext(io.s_axis_read_des.fire) || (!trCycle.orR && io.s_axi_mm2s.r.fire))//1、接收到描述符 2、当前传输已经完成 正在传输最后一个周期并且接收到了fire
  fsm.startMove := (fifo.io.availability > trLenCycle)//fifo可以全部接收，那么就开启下一次传输
  fsm.startEnd := !trDesc.len.orR && !trCycle.orR && io.s_axi_mm2s.r.fire //已经传输完成全部的数据，并且最后一共数据已经被接收
  fsm.popEnd := !fifo.io.occupancy.orR

  io.s_axis_read_des.ready.setAsReg() init False setWhen (fsm.isEntering(fsm.IDLE)) clearWhen (io.s_axis_read_des.fire)
  //数据读取和地址使能，突发传输开始
  io.s_axi_mm2s.ar.payload.addr := RegNextWhen(trDesc.addr, fsm.isExiting(fsm.PARA), U(0))
  io.s_axi_mm2s.ar.payload.setBurstINCR()//突发类型递增
  io.s_axi_mm2s.ar.payload.setSize(config.maxSingleBrustSize)
  io.s_axi_mm2s.ar.payload.id := U(1, config.axiConfig.idWidth bits)
  io.s_axi_mm2s.ar.payload.setCache(B"4'b0011")
  io.s_axi_mm2s.ar.payload.setLock(B"1'b0") //不锁定总线
  io.s_axi_mm2s.ar.payload.setProt(B"3'b010")
  io.s_axi_mm2s.ar.payload.len := RegNextWhen(trLenCycle, fsm.isExiting(fsm.PARA), U(0))
  io.s_axi_mm2s.ar.valid setAsReg() init(False) clearWhen(io.s_axi_mm2s.ar.ready) setWhen(fsm.isExiting(fsm.PARA))

  io.s_axi_mm2s.r.ready := fsm.isActive(fsm.MOVE) && fifo.io.push.ready
  fifo.io.push.valid := io.s_axi_mm2s.r.valid && fsm.isActive(fsm.MOVE)
  fifo.io.push.payload.data := io.s_axi_mm2s.r.payload.data
  fifo.io.push.payload.keep.setAll()
  fifo.io.push.payload.last := !trDesc.len.orR && !trCycle.orR && io.s_axi_mm2s.r.fire

  fifo.io.pop <> io.m_axis_mm2s
}

object xg_axi_dma_read extends App {
  SpinalVerilog(new xg_axi_dma_read(DMA_CONFIG())).printPruned
}