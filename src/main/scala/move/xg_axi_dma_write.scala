package move

import spinal.lib.experimental.chisel.Bundle
import spinal.core.{U, UInt, _}
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4Aw, Axi4Config, Axi4WriteOnly}
import spinal.lib.bus.amba4.axis.Axi4Stream.{Axi4Stream, Axi4StreamBundle}
import spinal.lib.bus.amba4.axis.{Axi4Stream, Axi4StreamConfig}
import spinal.lib.fsm._
import wa.WaCounter
import spinal._
/*
分别对读口和写口做修改，
  以写口为例，得到的数据，当前数据量大于突发长度才会发一个突发。
  读口当前可用容量大于突发长度，才会发送一个突发。

  不支持 非对齐传输，以AXI DMA Width宽度为大小。
  不支持strb拉低需要组包的情况。

  write dma
  传输结束的标志为length描述符配置长度，和last信号。二者满足一个及代表传输结束。
  具体情况如下，
  以last为数据包标志位，代表正真的结束。
  1、length传输完成，last传输完成，数据量对齐：完美结束
  2、length传输完成，last没有接收到，那么进入丢弃数据状态，直到接收数据。
  3、length传输未完成，last已经接收到，那么传输完剩余数据结束。


  idle：空闲状态可以接收描述符
  para：准备状态，等待数据，发送写控制信号（负责计算地址，数据，突发长度），做一个准备以及切换，last提前拉高，那么修改突发传输传输完剩余数据结束。
  move：数据搬移状态，开始数据搬移，搬移length大小，起始地址addr
  drop_data:待搬移的数据为0，但是还为接收到last信号，或者last信号数据量大于所需的数据量，流程ready拉高等待last信号，last拉高后清空fifo
  flush_fifo:刷新缓冲区数据，刷新完成后回到idle状态。

  read dma
  以length为标志位，直到传输完成。
  计算每一次突发，当fifo可用容量大于突发长度即开始工作。最后一次需要给last信号。代表传输完成。
  接受开始信号的时候刷新fifo。

  idle：空闲状态，接收描述符
  para：准备状态，发送控制信号
  move：搬移状态，开始搬移数据
  end：接收结束信号，开始下一次发送
  一次循环，idle -> para -> move -> end -> para .... 一次准备状态
  进入move状态的条件是，1、数据量准备就绪
  异常处理情况，如果last提前拉高，那么进入move的条件判断需要进行修改，如果数据量不足，那么提前拉高数据
  异常处理情况，如果last一直不拉高，para状态进入drop状态，一直丢弃接收到的数据直到接收到last信号，开始进入idle状态。
  重复para和move切换。完成后发送状态描述符

  //总结
  每次完成传输后，即刷新fifo，清空全部数据。
*/

//
//
case class DMA_CONFIG(addrWidth:Int = 32, dataWidth:Int = 64, brustLength:Int = 16, fifoDepthWidth:Int = 5, idWidth:Int = 4){
  val axiConfig = Axi4Config(addrWidth,dataWidth, useRegion = false, useQos = false, idWidth = 4)
  val wordSize = 8
  val strbWidth = (dataWidth/wordSize)
  val axisConfig = Axi4StreamConfig(strbWidth, useKeep = true, useLast = true)

  val maxSingleBrustSize = log2Up(strbWidth)
  val maxBrustSize = brustLength << maxSingleBrustSize
  val min4kBrustSize = maxBrustSize.min(4096)
  println("min4kBrustSize:"+min4kBrustSize)
  val addrMask = ((BigInt(1) << addrWidth) - 1) << log2Up(strbWidth) mod (BigInt(1) << addrWidth)
  val fifoDepth = 1 << fifoDepthWidth

  println("addrWidth:" + addrWidth)
  println("dataWidth:" + dataWidth)
  println("brustLength:" + brustLength)
  println("fifoDepthWidth:" + fifoDepthWidth)
  println("strbWidth:" + strbWidth)
  println("maxSingleBrustSize:" + maxSingleBrustSize)
  println("maxBrustSize:" + maxBrustSize)
}
////fifo内存储的数据格式
//class dataSave(input:Axi4StreamBundle) extends Bundle(){
//  val data = input.data
//  val last = input.last
//  val strb = input.keep
//}
//启动描述符，包含起始地址和长度，起始地址必须对齐addr的位宽，dma回到idle状态后发送
//不支持位宽转换
case class axis_desc(config: DMA_CONFIG) extends Bundle{
  val addr = UInt(config.addrWidth bits)
  val len = UInt(26 bits)
  //以字节为长度
  def getNextIncDesc(trSizeCount:UInt): axis_desc ={
    val nextDesc = axis_desc(config)
    nextDesc.addr := this.addr + trSizeCount
    nextDesc.len := this.len - trSizeCount
    nextDesc
  }
  //待传输的数据量小于一次传输所需的突发容量
  def stBurstSize(): Bool = {
    len < config.min4kBrustSize
  }
  //判断搬移trSizeCount字节个数据是否，是否跨越了4k边界，如果跨越了4K边界
  def isCrossesBoundary(trSizeCount: UInt): Bool = {
    ((addr(11 downto 0)) +^ trSizeCount) >> 12 =/= U(0)
  }

  def get4kBoundarySize: UInt = U"13'h1000" - (addr(11 downto 0))

  def init(value:Int = 0): Unit ={
    addr.init(value)
    len.init(value)
  }
}

class axiDmaWriteFsm extends StateMachine {//分为四个状态，空闲，有效，和输出down的数据，END
  setEncoding(binaryOneHot)
  val IDLE = new State with EntryPoint //空闲状态
  val PARA = new State //准备状态
  val MOVE = new State //搬运数据状态
  val END = new State //结束状态
//  val DROP = new State //丢弃数据状态

  val startPara, startMove, startEnd,startPara2 = Bool()
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
      }
    }
  END
    .whenIsActive {
      when(startPara2){
        goto(PARA)
      } otherwise{
        goto(IDLE)
      }
    }
}

object computeTrSize{
  def apply(trDesc : axis_desc, config:DMA_CONFIG): UInt ={
    val trSizeCount = Reg(UInt(13 bits)) init 0
    when(trDesc.stBurstSize()){//当前一次传输的数据量小于4k边界并且小于最大突发长度
      when(trDesc.isCrossesBoundary(trDesc.len)){//跨越4k地址边界
        trSizeCount := trDesc.get4kBoundarySize
      } otherwise{//没有跨越4k地址边界
        trSizeCount := trDesc.len.resized
      }
    }otherwise{//大于4k地址边界或者大于最大突发长度
      when(trDesc.isCrossesBoundary(config.min4kBrustSize)){//跨越4k地址边界
        trSizeCount := trDesc.get4kBoundarySize
      } otherwise{//没有跨越4k地址边界
        trSizeCount := config.min4kBrustSize
      }
    }
    trSizeCount
  }
}

//支持4k地址变化，不支持位宽转换，如果需要位宽转换，先进行位宽转换在给到该模块
class xg_axi_dma_write(config : DMA_CONFIG) extends Component {
  val io = new Bundle{
    val m_axi_s2mm = master(Axi4WriteOnly(config.axiConfig))
    val s_axis_s2mm = slave(Axi4Stream(config.axisConfig))
    val s_axis_write_des = slave Stream axis_desc(config)
  }

  /**
   * 1、fifo可用容量大小：
   * 2、fifo内部是否存在last信号
   * 3、当前要发送的数据字节个数大小
   * 4、输出周期
   */
  val fsm = new axiDmaWriteFsm()
  val fifo = StreamFifo(io.s_axis_s2mm.payloadType, config.fifoDepth)
  //分为以下几种情况，dropData直接丢弃数据、idle不接受数据、其余情况如果没有接受到全部数据那么就接受，如果已经接受了全部的数据就不在接受数据
  //结束标志为，last信号拉高 或者 已经接受了length个数据

  /**
   * 一次突发传输字节数计算，在开始状态时计算传输数据的地址和长度
   * 1、跨越4k边界
   * 2、大于突发传输大小
   * 3、小于突发传输大小和突发容量
   *
   * 计算需要发送的周期数
   */
  val lenDescReg = axis_desc(config)
  val trCycleNext, trCycleReg = UInt(8 bits)
  val fifoGetLast = Reg(Bool()) init False setWhen(io.s_axis_s2mm.fire && io.s_axis_s2mm.last) clearWhen(fsm.isActive(fsm.IDLE))
  //考虑以下情况，1、当前fifo内部容量大于当前传输的数据 2、已经接收到last信号
  //启动突发传输的最小的数据量（单位：字节）
  //选择当前正确传输的数据长度
  //计算描述符的起始地址和数据长度
  lenDescReg.setAsReg() init(0)
  val descAddSize = UInt(13 bits)
  when(fsm.isEntering(fsm.MOVE)){
    lenDescReg := lenDescReg.getNextIncDesc(descAddSize)
  }

  when(io.s_axis_write_des.fire){
    lenDescReg := io.s_axis_write_des.payload
  }

  val trLenSize = computeTrSize(lenDescReg, config)//len传输的字节数量
  val trFifoSize = (fifo.io.occupancy << config.maxSingleBrustSize)//fifo内可以传输的字节数
  val trLenCycle = ((trLenSize - 1) >> config.maxSingleBrustSize).resize(8)//传输的周期数 - 1
  val trFifoCycle = (fifo.io.occupancy - 1)//fifo内可以传输的周期数
  //两种情况进行选择
  //最终的传输长度,包含两种情况 1、last提取结束，2、按照len正常结束
  val selFifo = ((trLenCycle > trFifoCycle) && fifoGetLast)
  val selTrCycle = (selFifo).mux(trFifoCycle, trLenCycle)
  val selTrSize =  (selFifo).mux(trFifoSize, trLenSize)
  //当前传输的起始地址和需要传输的数据长度
  //选择需要传输的字节数量,如果已经接收到last信号，并且当前fifo可用容量小于lenTrSIze的容量。那么选择fifoTrSize
  descAddSize := selTrSize
  //开始传输的时候，得到新的长度。
  //传输的周期数
  trCycleReg := RegNext(trCycleNext, U(0, trCycleNext.getWidth bits))
  //传输周期的逻辑判断，突发传输开始计算传输次数，每次传输完成一次，记录减少一次传输次数
  when(fsm.isEntering(fsm.MOVE)){
    trCycleNext := selTrCycle
  } elsewhen(io.m_axi_s2mm.w.fire){
    trCycleNext := trCycleReg - 1
  } otherwise{
    trCycleNext := trCycleReg
  }
  //跳转使能的逻辑判断：当前fifo占用容量大于传输周期代表数据量充足可用使用

  //当前fifo容量大于所需传输的周期数，代表数据量充足，可用开启一次突发传输
  //或者当前是last使能，
  fsm.startMove := (fifo.io.occupancy > selTrCycle) || ((fifo.io.occupancy === selTrCycle) && io.s_axis_s2mm.valid)//当然数据量足够，开始传输
  fsm.startEnd  := !trCycleReg.orR && (!io.m_axi_s2mm.w.valid || io.m_axi_s2mm.w.ready) && (!io.m_axi_s2mm.aw.valid || io.m_axi_s2mm.aw.ready)
  fsm.startPara := RegNext(io.s_axis_write_des.fire)//等待一个周期计算发送size
  fsm.startPara2 := (!fifoGetLast || fifo.io.occupancy.orR) && lenDescReg.len.orR

  //描述符的接收
  io.s_axis_write_des.ready.setAsReg() init True setWhen(fsm.isEntering(fsm.IDLE)) clearWhen(io.s_axis_write_des.valid)
  //数据的接收
  val s2mmLength = RegNextWhen((io.s_axis_write_des.len - 1) >> config.maxSingleBrustSize, io.s_axis_write_des.fire, U(0))
  val s2mmCount = WaCounter(io.s_axis_s2mm.fire, 14, s2mmLength)
  //停止数据输入，的判断依据两条，1、接收到last信号，2、已经接收len个数据
  val continueInput = RegInit(True) setWhen (fsm.isExiting(fsm.IDLE)) clearWhen (s2mmCount.validLast() || (io.s_axis_s2mm.fire && io.s_axis_s2mm.last))
  io.s_axis_s2mm.continueWhen(continueInput) <> fifo.io.push //如果已经接收了length个数据，或者last信号那么就不在接收

  //控制通道的数据发送
  io.m_axi_s2mm.aw.valid.setAsReg() init False clearWhen (io.m_axi_s2mm.aw.ready) setWhen(fsm.isEntering(fsm.MOVE))
  io.m_axi_s2mm.aw.payload.addr := RegNextWhen(lenDescReg.addr, fsm.isEntering(fsm.MOVE), U(0))
  io.m_axi_s2mm.aw.payload.setBurstINCR()//突发类型递增
  io.m_axi_s2mm.aw.payload.setSize(config.maxSingleBrustSize)//突发字节数
  io.m_axi_s2mm.aw.payload.setCache(B"4'b0011") //写入可使用buff缓存
  io.m_axi_s2mm.aw.payload.setLock(B"1'b0") //不锁定总线
  io.m_axi_s2mm.aw.payload.setProt(B"3'b010")
  io.m_axi_s2mm.aw.id := U(0, config.axiConfig.idWidth bits)
  io.m_axi_s2mm.aw.payload.len := RegNextWhen(trCycleNext, fsm.isEntering(fsm.MOVE), U(0))

  //写通道的数据发送
  io.m_axi_s2mm.w.valid.setAsReg() init False clearWhen(!trCycleReg.orR && io.m_axi_s2mm.w.ready) setWhen (fsm.isEntering(fsm.MOVE))
  io.m_axi_s2mm.w.payload.data := fifo.io.pop.payload.data
  io.m_axi_s2mm.w.payload.strb := fifo.io.pop.payload.keep
  io.m_axi_s2mm.w.payload.last := trCycleReg === U(0)

  //写响应通道
  val retNumNext = UInt(10 bits)
  val retNumReg = RegNext(retNumNext) init 0
  when(io.m_axi_s2mm.aw.fire ^ io.m_axi_s2mm.b.fire){
    when(io.m_axi_s2mm.aw.fire){
      retNumNext := retNumReg + 1
    } otherwise{
      retNumNext := retNumReg - 1
    }
  }otherwise {
    retNumNext := retNumReg
  }
  io.m_axi_s2mm.b.ready := retNumReg.orR
  fifo.io.pop.ready := io.m_axi_s2mm.w.fire
}

object xg_axi_dma_write extends App {
  SpinalVerilog(new xg_axi_dma_write(DMA_CONFIG(dataWidth = 512))).printPruned
}