package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import wa.ip._
import spinal.lib.experimental.chisel.Module
import spinal.core.sim._
case class rotateConfig(DATA_NUM : Int = 2,
                      MEM_DEPTH : Int = 1024,
                      SIZE_WIDTH : Int = 11,
                        computeNum : Int = 32
                     ) {
    //创建的MEM个数
}

class Rotate(config:rotateConfig) extends Module{
    val io = new Bundle {//要求valid有效之后数据需要保持8周期的有效值//
        val sDataTan = slave Flow UInt(5 bits) //求出选择角度
        val sDataBrief = slave Flow Bits(config.computeNum bits)//传出M01和M10两个数据
        val mDataRsBrief = master Stream Bits(config.computeNum * 2 bits)
        val sReady = out Bool()
    }
    //1、接收数据
    val fifoTan = StreamFifo(Bits(io.sDataTan.payload.getWidth bits), 16)
    val fifoBrief = StreamFifo(Bits(io.sDataBrief.payload.getWidth bits), 8 * 16)
    fifoTan.io.push.payload := io.sDataTan.payload.asBits
    fifoTan.io.push.valid := io.sDataTan.valid
    fifoBrief.io.push.payload := io.sDataBrief.payload
    fifoBrief.io.push.valid := io.sDataBrief.valid

    fifoTan.io.availability.simPublic()
    //2、根据接收的数据开启传输
    //2、接收Tan的值,Stream正常传输数据，tan0HaveData代表有没有数据，
    val tan0 = Flow(UInt(5 bits))
    val tan0HaveData, tan1HaveData = Reg(Bool()) init False
    val tanCount,tanCount1  = Reg(UInt(3 bits)) init(0)
    val selCount = Reg(UInt(3 bits)) init(0)
    val selCount1 = Reg(UInt(2 bits)) init(0)
    fifoTan.io.pop.ready := (!tan0HaveData || tan0.fire) && (fifoBrief.io.occupancy > 7)
    when(fifoTan.io.pop.fire){
        selCount := (8 - fifoTan.io.pop.payload(2 to 4).asUInt)(0 to 2)
    } elsewhen(tan0HaveData){
        selCount := selCount + 1
    }
    when(fifoTan.io.pop.fire){
        tan0HaveData := True
        tan0.payload := fifoTan.io.pop.payload.asUInt
    } elsewhen(tan0.valid){
        tan0HaveData := False
    }
    when(tan0HaveData){
        tanCount := tanCount + 1
    }
    tan0.valid := tan0HaveData & tanCount.andR
    tan0.valid.setAsReg()
    tan0.payload.setAsReg()
    when(tan0.valid){
        tan1HaveData := True
    } elsewhen(tan1HaveData & tanCount1.andR){
        tan1HaveData := False
    }
    when(tan1HaveData) {
        tanCount1 := tanCount1 + 1
    }
    when(tan0.valid) {
        selCount1 := tan0.payload(0 to 1)
    } elsewhen (tan1HaveData && selCount1 > 0) {
        selCount1 := selCount1 - 1
    }

    //3、接收Brief的值，并对这个值进行旋转
    val brief0 = Flow(Vec(Bits(256 / 8 bits), 8))
    val brief1 = Flow(Vec(Bits(8 bits), 256 / 8))
    val brief0HaveData = Reg(Bool()) init False
    val briefCount0 = Reg(UInt(3 bits)) init (0)
    fifoBrief.io.pop.ready := RegNext(fifoTan.io.pop.fire, False) || briefCount0.orR
//    fifoBrief.io.pop.ready.setAsReg() init(False)
//    when(fifoTan.io.pop.fire){
//        fifoBrief.io.pop.ready := True
//    } elsewhen (tanCount.andR){
//        fifoBrief.io.pop.ready := False
//    }
    when(fifoBrief.io.pop.fire && !briefCount0.orR) {
        brief0HaveData := True
    } elsewhen (brief0.valid) {
        brief0HaveData := False
    }
    when(fifoBrief.io.pop.fire){
        briefCount0 := briefCount0 + 1
    }
    when(fifoBrief.io.pop.fire && briefCount0.andR){
        brief0.valid := True
    } elsewhen(brief0.valid){
        brief0.valid := False
    }
    brief0.valid.setAsReg()
    switch(selCount){
        for(i <- 0 to 7){
            brief0.payload(i).setAsReg() init 0
            is(i){
                when(fifoBrief.io.pop.fire){
                    brief0.payload(i) := fifoBrief.io.pop.payload
                }
            }
        }
    }
    when(RegNext(tan0.fire)) {
        brief1.payload := brief0.payload.asBits.subdivideIn(32 slices)
    }

    val fifoRsBrief = StreamFifo(Bits(64 bits), 128)
    brief1.payload.foreach(_.setAsReg() init(0))
    when(RegNext(selCount1 =/= 0 & tan1HaveData & tanCount1 < 4)){//如果满足的话就位移
        for(i <- 0 to 31){
            brief1.payload(i) := brief1.payload((i + 1) % 32)
        }
    }
    switch(tanCount1(0 to 1)){
        for(i <- 0 to 3){
            is(i) {
                for(j <- 0 to 7){
                    fifoRsBrief.io.push.payload.subdivideIn(8 slices)(j) := brief1.payload(i * 8 + j)
                }
            }
        }
    }
    fifoRsBrief.io.push.valid := tanCount1 >= 4
    fifoRsBrief.io.pop <> io.mDataRsBrief
    io.sReady := fifoRsBrief.io.availability >= 20
}


object Rotate extends App {
    SpinalVerilog(new Rotate(rotateConfig()))
}