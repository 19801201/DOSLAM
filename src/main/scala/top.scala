//这是所有模块的顶层
import operator.NMSConfig
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import operator._
import spinal.core.Component.push
import data._
import dataStructure.{FeaturePoint, FeaturePointOrb, OrbFpRsIO}
import instruction.Instruction
import move._
import spinal.lib.bus.amba4.axi.{Axi4ReadOnly, Axi4WriteOnly}
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.experimental.chisel.Module
import utils.{ImageCount, ImageSize}
import spinal.lib.experimental.chisel.Module
import top._

case class TopConfig(fastType:String = FAST_TYPE.small, MEM_DEPTH : Int = 128, SIZE_WIDTH : Int = 11, TopSort:Int = -1, isBlock : Boolean = false, BSNum : Int = -1){
  val orbConfig = ORB_ComputeConfig(fastType, MEM_DEPTH, SIZE_WIDTH, TopSort, isBlock, BSNum)
  val readDmaConfig = DMA_CONFIG(dataWidth = 64, brustLength = 256, fifoDepthWidth = 9)
  val writeDmaImageConfig = DMA_CONFIG(dataWidth = 64)
  val writeDmaOrbConfig = DMA_CONFIG(dataWidth = 512)
}

class Top(config: TopConfig) extends Module {
  val io = new Bundle{
    val regSData = slave(AxiLite4(log2Up(1 MiB), 32))
    val AXI_mm2s_image = master(Axi4ReadOnly(config.readDmaConfig.axiConfig)) //图片读
    val AXI_s2mm_image = master(Axi4WriteOnly(config.writeDmaImageConfig.axiConfig)) //图片写
    val AXI_s2mm_orb = master(Axi4WriteOnly(config.writeDmaOrbConfig.axiConfig)) //特征点读取
  }
  //三个计数器是其一，
  val inputCnt = WaCounter(io.AXI_mm2s_image.r.fire, 32, 640*480+100)
  val outputCnt = WaCounter(io.AXI_s2mm_image.w.fire, 32, 640*480+100)
  val outputFpCnt = WaCounter(io.AXI_s2mm_orb.w.fire, 32, 640*480+100)
  //每个通道的valid和ready去检查他的一个状态


  val orb = new ORB_Compute(config.orbConfig)
  val rdImageDma = new xg_axi_dma_read2(config.readDmaConfig)
  val wrImageDma = new xg_axi_dma_write(config.writeDmaImageConfig)
  val wrOrbDma = new xg_axi_dma_write(config.writeDmaOrbConfig)
  val instruction = new Instruction(config.readDmaConfig, config.writeDmaImageConfig, config.writeDmaOrbConfig)

  instruction.io.debugInstruction(0) := inputCnt.count.asBits
  instruction.io.debugInstruction(1) := outputCnt.count.asBits
  instruction.io.debugInstruction(2) := outputFpCnt.count.asBits

  instruction.io.debugInstruction(3) := rdImageDma.io.m_axis_mm2s.valid.asBits(32 bits)
  instruction.io.debugInstruction(4) := wrImageDma.io.s_axis_s2mm.valid.asBits(32 bits)
  instruction.io.debugInstruction(5) := wrOrbDma.io.s_axis_s2mm.valid.asBits(32 bits)

  instruction.io.debugInstruction(6) := rdImageDma.io.m_axis_mm2s.ready.asBits(32 bits)
  instruction.io.debugInstruction(7) := wrImageDma.io.s_axis_s2mm.ready.asBits(32 bits)
  instruction.io.debugInstruction(8) := wrOrbDma.io.s_axis_s2mm.ready.asBits(32 bits)
  //数据流入
  orb.io.sData.valid := rdImageDma.io.m_axis_mm2s.valid
  orb.io.sData.payload := rdImageDma.io.m_axis_mm2s.payload.data
  rdImageDma.io.m_axis_mm2s.ready := orb.io.sData.ready
  //图像流出
  wrImageDma.io.s_axis_s2mm.valid := orb.io.mDataImage.valid
  wrImageDma.io.s_axis_s2mm.payload.data := orb.io.mDataImage.payload
  wrImageDma.io.s_axis_s2mm.payload.keep.setAll()
  wrImageDma.io.s_axis_s2mm.payload.last.clear()
  orb.io.mDataImage.ready := wrImageDma.io.s_axis_s2mm.ready
  //特征点流出
  orb.io.mData.ready := wrOrbDma.io.s_axis_s2mm.ready
  wrOrbDma.io.s_axis_s2mm.valid := orb.io.mData.valid
  wrOrbDma.io.s_axis_s2mm.payload.data := B(0, wrOrbDma.io.s_axis_s2mm.payload.data.getWidth - (3 * 32) - orb.io.mData.payload.rs.getWidth bits) ## orb.io.mData.payload.rs.asBits ## orb.io.mData.payload.fp.size.rowNum.resize(32) ## orb.io.mData.payload.fp.size.colNum.resize(32) ## orb.io.mData.payload.fp.score.resize(32)
  wrOrbDma.io.s_axis_s2mm.payload.keep.setAll()
  wrOrbDma.io.s_axis_s2mm.payload.last := orb.io.mDataLast
  //特征点控制数据
  orb.io.start             := instruction.io.ORBInstruction(0)(0)
  orb.io.sizeIn.rowNum     := instruction.io.ORBInstruction(1).asUInt.resized
  orb.io.sizeIn.colNum     := instruction.io.ORBInstruction(2).asUInt.resized
  orb.io.threshold         := instruction.io.ORBInstruction(3).asUInt.resized
  orb.io.maskF             := instruction.io.ORBInstruction(4).resized
  orb.io.maskR             := instruction.io.ORBInstruction(5).resized
  orb.io.maskG             := instruction.io.ORBInstruction(6).resized
  orb.io.colNumFlilterIn   := instruction.io.ORBInstruction(7).asUInt.resized
  orb.io.rowNumSrcIn       := instruction.io.ORBInstruction(8).asUInt.resized
  orb.io.colNumSrcIn       := instruction.io.ORBInstruction(9).asUInt.resized
  orb.io.inValid           := instruction.io.ORBInstruction(10).resized
  orb.io.topNum            := instruction.io.ORBInstruction(11).asUInt.resized
  if(config.isBlock){
    orb.io.thresholdInit     := instruction.io.ORBInstruction(12).asUInt.resized
  }

  instruction.io.ORBInstructionIn(0) := orb.io.inputLength.asBits.resized
  instruction.io.ORBInstructionIn(1) := orb.io.outputLength.asBits.resized
  //axi dma 描述符
  rdImageDma.io.s_axis_read_des <> instruction.io.dmaImageRead
  wrImageDma.io.s_axis_write_des <> instruction.io.dmaImageWrite
  wrOrbDma.io.s_axis_write_des <> instruction.io.dmaOrbWrite
  //输入和输出
  io.AXI_mm2s_image <> rdImageDma.io.s_axi_mm2s
  io.AXI_s2mm_image <> wrImageDma.io.m_axi_s2mm
  io.AXI_s2mm_orb <> wrOrbDma.io.m_axi_s2mm
  io.regSData <> instruction.io.regSData
  //修改名称
  io.regSData.aw.valid.setName("regSData_axil_awvalid")
  io.regSData.aw.ready.setName("regSData_axil_awready")
  io.regSData.aw.payload.addr.setName("regSData_axil_awaddr")
  io.regSData.aw.payload.prot.setName("regSData_axil_awprot")

  io.regSData.w.valid.setName("regSData_axil_wvalid")
  io.regSData.w.ready.setName("regSData_axil_wready")
  io.regSData.w.payload.data.setName("regSData_axil_wdata")
  io.regSData.w.payload.strb.setName("regSData_axil_wstrb")

  io.regSData.b.valid.setName("regSData_axil_bvalid")
  io.regSData.b.ready.setName("regSData_axil_bready")
  io.regSData.b.payload.resp.setName("regSData_axil_bresp")

  io.regSData.ar.valid.setName("regSData_axil_arvalid")
  io.regSData.ar.ready.setName("regSData_axil_arready")
  io.regSData.ar.payload.addr.setName("regSData_axil_araddr")
  io.regSData.ar.payload.prot.setName("regSData_axil_arprot")

  io.regSData.r.valid.setName("regSData_axil_rvalid")
  io.regSData.r.ready.setName("regSData_axil_rready")
  io.regSData.r.payload.data.setName("regSData_axil_rdata")
  io.regSData.r.payload.resp.setName("regSData_axil_rresp")

  io.AXI_s2mm_image.aw.valid.setName("writeImage_axi_awvalid")
  io.AXI_s2mm_image.aw.ready.setName("writeImage_axi_awready")
  io.AXI_s2mm_image.aw.payload.addr.setName("writeImage_axi_awaddr")
  io.AXI_s2mm_image.aw.payload.id.setName("writeImage_axi_awid")
  io.AXI_s2mm_image.aw.payload.len.setName("writeImage_axi_awlen")
  io.AXI_s2mm_image.aw.payload.size.setName("writeImage_axi_awsize")
  io.AXI_s2mm_image.aw.payload.burst.setName("writeImage_axi_awburst")
  io.AXI_s2mm_image.aw.payload.lock.setName("writeImage_axi_awlock")
  io.AXI_s2mm_image.aw.payload.cache.setName("writeImage_axi_awcache")
  io.AXI_s2mm_image.aw.payload.prot.setName("writeImage_axi_awprot")

  io.AXI_s2mm_image.w.valid.setName("writeImage_axi_wvalid")
  io.AXI_s2mm_image.w.ready.setName("writeImage_axi_wready")
  io.AXI_s2mm_image.w.payload.data.setName("writeImage_axi_wdata")
  io.AXI_s2mm_image.w.payload.strb.setName("writeImage_axi_wstrb")
  io.AXI_s2mm_image.w.payload.last.setName("writeImage_axi_wlast")

  io.AXI_s2mm_image.b.valid.setName("writeImage_axi_bvalid")
  io.AXI_s2mm_image.b.ready.setName("writeImage_axi_bready")
  io.AXI_s2mm_image.b.payload.id.setName("writeImage_axi_bid")
  io.AXI_s2mm_image.b.payload.resp.setName("writeImage_axi_bresp")

  io.AXI_s2mm_orb.aw.valid.setName("writeOrb_axi_awvalid")
  io.AXI_s2mm_orb.aw.ready.setName("writeOrb_axi_awready")
  io.AXI_s2mm_orb.aw.payload.addr.setName("writeOrb_axi_awaddr")
  io.AXI_s2mm_orb.aw.payload.id.setName("writeOrb_axi_awid")
  io.AXI_s2mm_orb.aw.payload.len.setName("writeOrb_axi_awlen")
  io.AXI_s2mm_orb.aw.payload.size.setName("writeOrb_axi_awsize")
  io.AXI_s2mm_orb.aw.payload.burst.setName("writeOrb_axi_awburst")
  io.AXI_s2mm_orb.aw.payload.lock.setName("writeOrb_axi_awlock")
  io.AXI_s2mm_orb.aw.payload.cache.setName("writeOrb_axi_awcache")
  io.AXI_s2mm_orb.aw.payload.prot.setName("writeOrb_axi_awprot")

  io.AXI_s2mm_orb.w.valid.setName("writeOrb_axi_wvalid")
  io.AXI_s2mm_orb.w.ready.setName("writeOrb_axi_wready")
  io.AXI_s2mm_orb.w.payload.data.setName("writeOrb_axi_wdata")
  io.AXI_s2mm_orb.w.payload.strb.setName("writeOrb_axi_wstrb")
  io.AXI_s2mm_orb.w.payload.last.setName("writeOrb_axi_wlast")

  io.AXI_s2mm_orb.b.valid.setName("writeOrb_axi_bvalid")
  io.AXI_s2mm_orb.b.ready.setName("writeOrb_axi_bready")
  io.AXI_s2mm_orb.b.payload.id.setName("writeOrb_axi_bid")
  io.AXI_s2mm_orb.b.payload.resp.setName("writeOrb_axi_bresp")

  io.AXI_mm2s_image.r.valid
  io.AXI_mm2s_image.ar.valid.setName("writeImage_axi_arvalid")
  io.AXI_mm2s_image.ar.ready.setName("writeImage_axi_arready")
  io.AXI_mm2s_image.ar.payload.addr.setName("writeImage_axi_araddr")
  io.AXI_mm2s_image.ar.payload.id.setName("writeImage_axi_arid")
  io.AXI_mm2s_image.ar.payload.len.setName("writeImage_axi_arlen")
  io.AXI_mm2s_image.ar.payload.size.setName("writeImage_axi_arsize")
  io.AXI_mm2s_image.ar.payload.burst.setName("writeImage_axi_arburst")
  io.AXI_mm2s_image.ar.payload.lock.setName("writeImage_axi_arlock")
  io.AXI_mm2s_image.ar.payload.cache.setName("writeImage_axi_arcache")
  io.AXI_mm2s_image.ar.payload.prot.setName("writeImage_axi_arprot")

  io.AXI_mm2s_image.r.valid.setName("writeImage_axi_rvalid")
  io.AXI_mm2s_image.r.ready.setName("writeImage_axi_rready")
  io.AXI_mm2s_image.r.payload.data.setName("writeImage_axi_rdata")
  io.AXI_mm2s_image.r.payload.id.setName("writeImage_axi_rid")
  io.AXI_mm2s_image.r.payload.resp.setName("writeImage_axi_rresp")
  io.AXI_mm2s_image.r.payload.last.setName("writeImage_axi_rlast")

  instruction.io.debugInstruction2 <> orb.io.debugInstruction
  instruction.io.debugInstruction3 <> orb.io.debugInstruction2

  val inputCnt1 = WaCounter(orb.io.sData.fire, 32, 640*480+100)
  val outputCnt1 = WaCounter(orb.io.mDataImage.fire, 32, 640*480+100)

  val startRecode1 = Reg(Bool()) init(False)
  when(orb.io.start.rise()){
    startRecode1.set()
  } elsewhen(inputCnt1.count === U(640 * 480 / 8 ,32 bits)){
    startRecode1.clear()
  }

  val startRecode2 = Reg(Bool()) init(False)
  when(orb.io.start.rise()){
    startRecode2.set()
  } elsewhen(outputCnt1.count === U(24576 ,32 bits)){
    startRecode2.clear()
  }

  val startRecode3 = Reg(Bool()) init(False)
  when(orb.io.start.rise()){
    startRecode3.set()
  } elsewhen(orb.io.mDataLast){
    startRecode3.clear()
  }

  val inputIdleCnt = WaCounter(startRecode1 && !orb.io.sData.valid && orb.io.sData.ready, 32, 640*480*10)
  val inputWaitCnt = WaCounter(startRecode1 && orb.io.sData.valid && !orb.io.sData.ready, 32, 640*480*10)
  val inputNoFireCnt = WaCounter(startRecode1 && !orb.io.sData.valid && !orb.io.sData.ready, 32, 640*480*10)

  val outputIdleCnt = WaCounter(startRecode2 && !orb.io.mDataImage.valid && orb.io.mDataImage.ready, 32, 640*480*10)
  val outputWaitCnt = WaCounter(startRecode2 && orb.io.mDataImage.valid && !orb.io.mDataImage.ready, 32, 640*480*10)
  val outputNoFireCnt = WaCounter(startRecode2 && !orb.io.mDataImage.valid && !orb.io.mDataImage.ready, 32, 640*480*10)

  val fpIdleCnt = WaCounter(startRecode3 && !orb.io.mData.valid && orb.io.mData.ready, 32, 640*480*10)
  val fpWaitCnt = WaCounter(startRecode3 && orb.io.mData.valid && !orb.io.mData.ready, 32, 640*480*10)
  val fpNoFireCnt = WaCounter(startRecode3 && !orb.io.mData.valid && !orb.io.mData.ready, 32, 640*480*10)

  when(instruction.io.debugClear.rise(False)){
    inputIdleCnt.clear
    inputWaitCnt.clear
    inputNoFireCnt.clear
    outputIdleCnt.clear
    outputWaitCnt.clear
    outputNoFireCnt.clear
    fpIdleCnt.clear
    fpWaitCnt.clear
    fpNoFireCnt.clear

    inputCnt.clear
    outputCnt.clear
    outputFpCnt.clear
  }

  instruction.io.debugInstruction4(0) := inputIdleCnt.count.asBits
  instruction.io.debugInstruction4(1) := inputWaitCnt.count.asBits
  instruction.io.debugInstruction4(2) := inputNoFireCnt.count.asBits
  instruction.io.debugInstruction4(3) := outputIdleCnt.count.asBits
  instruction.io.debugInstruction4(4) := outputWaitCnt.count.asBits
  instruction.io.debugInstruction4(5) := outputNoFireCnt.count.asBits
  instruction.io.debugInstruction4(6) := fpIdleCnt.count.asBits
  instruction.io.debugInstruction4(7) := fpWaitCnt.count.asBits
  instruction.io.debugInstruction4(8) := fpNoFireCnt.count.asBits

  instruction.io.debugClear <> orb.io.debugClear
}

object Top extends App {
  SpinalVerilog(new Top(TopConfig(FAST_TYPE.small, 128, 11, 256, isBlock = false, BSNum = -1))).printPruned
}