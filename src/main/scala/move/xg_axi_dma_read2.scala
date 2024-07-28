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

class axiDmaRead2Fsm extends StateMachine {//分为四个状态，空闲，有效，和输出down的数据，END
  setEncoding(binaryOneHot)
  val IDLE = new State with EntryPoint //空闲状态
  val PARA_MOVE = new State
  val MOVE = new State //得到数据进行搬运
  val END = new State //等待fifo数据全部派出

  val startPara_move, startMove, startEnd, popEnd = Bool()
  IDLE
    .whenIsActive {
      when(startPara_move) {
        goto(PARA_MOVE)
      }
    }
  PARA_MOVE//同时搬运和发出请求
    .whenIsActive {
      when(startEnd){
        goto(END)
      } elsewhen (startMove){
        goto(MOVE)
      }
    }
  MOVE
    .whenIsActive {
      when(startEnd){
        goto(END)
      }
    }
  END
    .whenIsActive {
      when(popEnd){
        goto(IDLE)
      }
    }
}

class xg_axi_dma_read2(config:DMA_CONFIG) extends Module{
  val io = new Bundle{
    val s_axi_mm2s = master(Axi4ReadOnly(config.axiConfig))
    val m_axis_mm2s = master(Axi4Stream(config.axisConfig))
    val s_axis_read_des = slave Stream axis_desc(config)
  }
  val fsm = new axiDmaRead2Fsm()
  val fifo = StreamFifo(io.m_axis_mm2s.payloadType, config.fifoDepth)

  //传输的起始地址和剩余传输的数据量的描述
  val trDesc = axis_desc(config) setAsReg()
  //判断每次需要传输的数据量和时钟数
  val trLenSize = computeTrSize(trDesc, config, false)//len传输的字节数量
  val trLenCycle = ((trLenSize - 1) >> config.maxSingleBrustSize).resize(8)//传输的周期数 - 1
  //修改传输的起始地址和剩余字节长度
  when(io.s_axis_read_des.fire){
    trDesc := io.s_axis_read_des.payload
  } elsewhen(io.s_axi_mm2s.ar.fire){
    trDesc := trDesc.getNextIncDesc(trLenSize)
  }
  //当前的剩余周期数目
  val trCycle = Reg(UInt(8 bits))
  when(fsm.isActive(fsm.PARA_MOVE)){
    trCycle := trLenCycle
  } elsewhen (io.s_axi_mm2s.r.fire){
    trCycle := trCycle - 1
  }

  val totalTrLenCycle = Reg(UInt(io.s_axis_read_des.payload.len.getWidth bits)) init(0)
  when(io.s_axis_read_des.fire){
    totalTrLenCycle := (io.s_axis_read_des.payload.len - 1) |>> config.maxSingleBrustSize
  } elsewhen(io.s_axi_mm2s.r.fire){
    totalTrLenCycle := totalTrLenCycle - 1
  }
  //判断能否进行传输
  val paraEnd = RegInit(False)
  val moveEnd = totalTrLenCycle === U(0, totalTrLenCycle.getWidth bits) && io.s_axi_mm2s.r.fire

  when(fsm.startMove){
    paraEnd := True
  } elsewhen(fsm.isEntering(fsm.IDLE)){
    paraEnd := False
  }

  fsm.startPara_move := RegNext(io.s_axis_read_des.fire) init False
  fsm.startMove := (trLenSize.resized === trDesc.len) && io.s_axi_mm2s.ar.fire
  fsm.startEnd := moveEnd && (paraEnd || fsm.startMove)
  fsm.popEnd := !fifo.io.occupancy.orR

  io.s_axis_read_des.ready.setAsReg() init False setWhen (fsm.isEntering(fsm.IDLE)) clearWhen (io.s_axis_read_des.fire)
  //数据读取和地址使能，突发传输开始
  io.s_axi_mm2s.ar.payload.addr := trDesc.addr
  io.s_axi_mm2s.ar.payload.setBurstINCR()
  io.s_axi_mm2s.ar.payload.setSize(config.maxSingleBrustSize)
  io.s_axi_mm2s.ar.payload.id := U(1, config.axiConfig.idWidth bits)
  io.s_axi_mm2s.ar.payload.setCache(B"4'b0011")
  io.s_axi_mm2s.ar.payload.setLock(B"1'b0")
  io.s_axi_mm2s.ar.payload.setProt(B"3'b010")
  io.s_axi_mm2s.ar.payload.len := trLenCycle
  io.s_axi_mm2s.ar.valid := fsm.isActive(fsm.PARA_MOVE)

  io.s_axi_mm2s.r.ready := (fsm.isActive(fsm.MOVE) || fsm.isActive(fsm.PARA_MOVE)) && fifo.io.push.ready
  fifo.io.push.valid := io.s_axi_mm2s.r.valid && (fsm.isActive(fsm.MOVE) || fsm.isActive(fsm.PARA_MOVE))
  fifo.io.push.payload.data := io.s_axi_mm2s.r.payload.data
  fifo.io.push.payload.keep.setAll()
  fifo.io.push.payload.last := !trDesc.len.orR && !trCycle.orR && io.s_axi_mm2s.r.fire

  fifo.io.pop <> io.m_axis_mm2s
}

object xg_axi_dma_read2 extends App {
  SpinalVerilog(new xg_axi_dma_read2(DMA_CONFIG(dataWidth = 512, brustLength = 16, fifoDepthWidth = 8))).printPruned
}