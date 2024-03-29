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
  val readDmaConfig = DMA_CONFIG(dataWidth = 64)
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

  val orb = new ORB_Compute(config.orbConfig)
  val rdImageDma = new xg_axi_dma_read(config.readDmaConfig)
  val wrImageDma = new xg_axi_dma_write(config.writeDmaImageConfig)
  val wrOrbDma = new xg_axi_dma_write(config.writeDmaOrbConfig)
  val instruction = new Instruction(config.readDmaConfig, config.writeDmaImageConfig, config.writeDmaOrbConfig)

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
  wrOrbDma.io.s_axis_s2mm.payload.data := B(0, wrOrbDma.io.s_axis_s2mm.payload.data.getWidth - 32 - orb.io.mData.payload.rs.getWidth bits) ## orb.io.mData.payload.rs.asBits ## orb.io.mData.payload.fp.asBits.resize(32)
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
}

object Top extends App {
  SpinalVerilog(new Top(TopConfig(FAST_TYPE.small, 128,11,-1,isBlock = false, BSNum = 3))).printPruned
}