package data

import spinal.core._
import spinal.core.sim.SimDataPimper
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
    //延迟一个周期
    val rowDataFlip = Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size)
    dataFlip(rowData, rowDataFlip, selKey(RegNextWhen(cnt.rowCnt.count, data.fire, 0)), BORDER_REFLECT101.getFlip(if(config.useFlip) config.WINDOWS_SIZE_H else 3), config.useFlip)
    //数据输入，生成水平数据，数据翻转
    val rdData = Flow(Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size))
    rdData.valid := data.fire
    rdData.payload := rowDataFlip
    val windowsReg = WindowsBuf(rdData, config.WINDOWS_SIZE_W)
    val colKey = selKey(RegNextWhen(cnt.colCnt.count, data.fire, 0))

    io.mData.payload.zipWithIndex.foreach((cur) => dataFlip(windowsReg(cur._2), cur._1, colKey, BORDER_REFLECT101.getFlip(if(config.useFlip) config.WINDOWS_SIZE_W else 3),config.useFlip))
    //如何控制valid和ready？
    data.ready := !io.mData.valid || io.mData.ready
    when(data.fire){
        io.mData.valid := dataValid && !(fsm.isActive(fsm.READY) && fsm.readyCnt.fireCnt)
    } elsewhen(io.mData.fire){
        io.mData.valid := False
    }
    io.mData.valid.setAsReg() init False
}
//修改一个bug，不能中断传输数据
class syncWindowsPadding2(config : WindowsConfig) extends Component{
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
    io.sizeOut.rowNum := io.sizeIn.rowNum
    io.sizeOut.colNum := io.sizeIn.colNum



    val data = weakCloneOf(io.sData)
    val fsm = fsmIVPPRF(io.start, data.fire ,io.mData.fire, io.sizeIn, config.WINDOWS_SIZE_H / 2, config.WINDOWS_SIZE_W / 2)

//    fsm.validCnt.colCnt.count.simPublic()
//    fsm.validCnt.rowCnt.count.simPublic()
    //状态机
    //---------------第一级流水数据控制--------------------//
    io.sData.ready := data.ready && fsm.isActive(fsm.VALID)
    //数据有效那么就接收
    data.payload := io.sData.payload
    data.valid := (fsm.isActive(fsm.VALID) && io.sData.valid) || fsm.isActive(fsm.PADDINGROW)|| fsm.isActive(fsm.PADDINGCOL)
    //1、数据正常传递，2、数据填充边界
    val rowData = RowBuf(data.toFlowFire, io.sizeIn.colNum, config.MEM_DEPTH, config.WINDOWS_SIZE_H)//延迟一个周期
    //数据缓存若干行
    val rowDataFlip = Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size)
    val cnt = new ImageCount(data.fire, io.sizeIn)//进行一个输入数据位置的标记
    dataFlip(rowData, rowDataFlip, selKey(RegNextWhen(cnt.rowCnt.count, data.fire, 0)), BORDER_REFLECT101.getFlip(if(config.useFlip) config.WINDOWS_SIZE_H else 3), config.useFlip)
    //对数据进行对称映射
    //---------------第二级流水数据控制--------------------//
    val rdData = Stream(Vec(Bits(config.DATA_STREAM_WIDTH bits), rowData.size))
    data.ready := (!rdData.valid || rdData.ready)
    //反压上一级
    rdData.valid.setAsReg() init False
    when(data.ready){
        rdData.valid := data.valid
    } elsewhen (rdData.ready){
        rdData.valid := False
    }
    rdData.payload := rowDataFlip
    val windowsReg = WindowsBufDelay(rdData.toFlowFire, config.WINDOWS_SIZE_W)//延迟一个周期
    //缓存W行
    val colKey = selKey(RegNextWhen(RegNextWhen(cnt.colCnt.count, data.fire, 0), rdData.fire, 0))
    io.mData.payload.zipWithIndex.foreach((cur) => dataFlip(windowsReg(cur._2), cur._1, colKey, BORDER_REFLECT101.getFlip(if(config.useFlip) config.WINDOWS_SIZE_W else 3),config.useFlip))
    //输出数据
    //先生成垂直的N行数据
    val dataValid = Reg(Bool) init False setWhen ((fsm.validCnt.rowCnt.count >= config.WINDOWS_SIZE_H / 2) && (fsm.validCnt.colCnt.count >= (config.WINDOWS_SIZE_W / 2)))
    dataValid.clearWhen(fsm.isActive(fsm.READY) && fsm.readyCnt.fireCnt)
    io.mData.valid.setAsReg() init False
    when(rdData.ready){
        io.mData.valid := rdData.valid && dataValid
    } elsewhen (io.mData.ready){
        io.mData.valid := False
    }
    rdData.ready := (!io.mData.valid || io.mData.ready) || (!dataValid)
    //清空不需要的缓冲数据
    cnt.clear(fsm.isActive(fsm.FLUSH))
}

object syncWindows extends App {
    SpinalVerilog(new syncWindows(WindowsConfig(DATA_NUM = 8, WINDOWS_SIZE_H = 31, WINDOWS_SIZE_W = 5, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}

object syncWindowsPadding extends App {
    SpinalVerilog(new syncWindowsPadding(WindowsConfig(DATA_NUM = 1, WINDOWS_SIZE_H = 7, WINDOWS_SIZE_W = 3, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}

object syncWindowsPadding2 extends App {
    SpinalVerilog(new syncWindowsPadding2(WindowsConfig(DATA_NUM = 1, WINDOWS_SIZE_H = 7, WINDOWS_SIZE_W = 3, MEM_DEPTH = 128, SIZE_WIDTH = 11)))
}
