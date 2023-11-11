package data

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import utils._
import wa.WaCounter

/*
做1级流水得windows窗口，输入经过一级寄存器之后直接输出。
 */
class syncWindows(config : WindowsConfig) extends Component{
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream (Bits(config.DATA_STREAM_WIDTH bits))
        val mData = master Stream (Vec(Vec(Bits(config.DATA_STREAM_WIDTH bits),config.WINDOWS_SIZE_W), config.WINDOWS_SIZE_H))
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号 传入和传出都是subOne
        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
        val sizeOut = master(new ImageSize(config.SIZE_WIDTH))
    }

    val fsm = fsmIV(io.start, io.sData, io.sizeIn)

    io.sizeOut := io.sizeIn.sub((config.WINDOWS_SIZE_H - 1), (config.WINDOWS_SIZE_W - 1))

    val rowDataValid0 = fsm.validCnt.rowCnt.count >= (config.WINDOWS_SIZE_H - 1)
    val colDataValid0 = fsm.validCnt.colCnt.count >= (config.WINDOWS_SIZE_W - 1)

    io.sData.ready := !io.mData.valid || io.mData.ready
    io.mData.valid.setAsReg() init False
    when(io.sData.fire) { //上游传递数据，那么数据取决于上游数据是否有效，
        io.mData.valid := io.sData.valid && rowDataValid0 && colDataValid0
    } elsewhen (io.mData.fire) { //下游接收数据，那么数据变为无效
        io.mData.valid := False
    }

    //io.sData.clearValidWhen(rowDataValid0 && colDataValid0).m2sPipe()

    val rdData = Flow(Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_H))
    rdData.payload := RowBuf(io.sData.toFlowFire, io.sizeIn.colNum, config.MEM_DEPTH, config.WINDOWS_SIZE_H)
    rdData.valid := io.sData.fire
    io.mData.payload := WindowsBuf(rdData, config.WINDOWS_SIZE_W)
}

//要求保持数据大小不变 不需要保证数据正确性
class syncWindowsPadding(config : WindowsConfig) extends Component{
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Stream (Bits(config.DATA_STREAM_WIDTH bits))
        val mData = master Stream (Vec(Vec(Bits(config.DATA_STREAM_WIDTH bits),config.WINDOWS_SIZE_W), config.WINDOWS_SIZE_H))
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val sizeIn = slave(new ImageSize(config.SIZE_WIDTH))
        val sizeOut = master(new ImageSize(config.SIZE_WIDTH))
    }

    def selKey(count: UInt, delayCount: Int = 0): UInt = { //根据位置选择不同的东西，
        val sel = UInt(5 bits)
        when(count >= config.WINDOWS_SIZE_H - 1) {
            sel := BORDER_REFLECT101.FlipDefault
        } otherwise {
            sel := count.resized
        }

        if (delayCount == 0) sel
        else Delay(sel, delayCount)
    }

    val data = weakCloneOf(io.sData)
    val fsm = fsmIVPRF(io.start, data.fire ,io.mData.fire, io.sizeIn, io.sizeOut, PADDING_NUM = config.WINDOWS_SIZE_H / 2)

    data.payload := io.sData.payload
    data.valid.allowOverride := (fsm.isActive(fsm.VALID) && io.sData.fire) || fsm.isActive(fsm.PADDING) || fsm.isActive(fsm.READY)
    io.sData.ready := data.ready && fsm.isActive(fsm.VALID)
    //填充到原本大小的数据
    io.sizeOut.rowNum := io.sizeIn.rowNum
    io.sizeOut.colNum := io.sizeIn.colNum
    //先生成垂直的N行数据
    val dataValid = Reg(Bool) init False setWhen ((fsm.validCnt.rowCnt.count >= config.WINDOWS_SIZE_H / 2) && (fsm.validCnt.colCnt.count >= (config.WINDOWS_SIZE_W / 2) - 1))
    dataValid.clearWhen(fsm.isActive(fsm.READY) && fsm.readyCnt.fireCnt)
    //数据输入，生成垂直数据，数据翻转
    val cnt = new ImageCount(data.fire, io.sizeIn)
    cnt.clear(fsm.isActive(fsm.FLUSH))
    val rowData = RowBuf(data.toFlowFire, io.sizeIn.colNum, config.MEM_DEPTH, config.WINDOWS_SIZE_H)
    val rowDataFlip = Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size)
    dataFlip(rowData, rowDataFlip, selKey(RegNextWhen(cnt.rowCnt.count, data.fire, 0)), BORDER_REFLECT101.getFlip(config.WINDOWS_SIZE_H))
    //数据输入，生成水平数据，数据翻转
    val rdData = Flow(Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size))
    rdData.valid := data.fire
    rdData.payload := rowDataFlip
    val windowsReg = WindowsBuf(rdData, config.WINDOWS_SIZE_W)
    val colKey = selKey(RegNextWhen(cnt.colCnt.count, data.fire, 0))

    io.mData.payload.zipWithIndex.foreach((cur) => dataFlip(windowsReg(cur._2), cur._1, colKey, BORDER_REFLECT101.getFlip(config.WINDOWS_SIZE_W)))
    //如何控制valid和ready？
    data.ready := !io.mData.valid || io.mData.ready
    when(data.fire){
        io.mData.valid := dataValid && !(fsm.isActive(fsm.READY) && fsm.readyCnt.fireCnt)
    } elsewhen(io.mData.fire){
        io.mData.valid := False
    }
    io.mData.valid.setAsReg() init False

}

object syncWindows extends App {
    SpinalVerilog(new syncWindows(WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 31, WINDOWS_SIZE_W = 5, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}

object syncWindowsPadding extends App {
    SpinalVerilog(new syncWindowsPadding(WindowsConfig(DATA_NUM = 1, WINDOWS_SIZE_H = 7, WINDOWS_SIZE_W = 3, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}
