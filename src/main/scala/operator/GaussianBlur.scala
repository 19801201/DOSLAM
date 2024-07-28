package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import spinal.lib.experimental.chisel.Module

case class GaussianBlurConfig(DATA_NUM : Int = 8,
                                      MEM_DEPTH : Int = 1024,
                                      SIZE_WIDTH : Int = 11
                                     ) {
    val WINDOWS_SIZE_H = 7
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    //创建的MEM个数
}

class GaussianBlur(config:GaussianBlurConfig) extends Module{
    val io = new Bundle {
        //增加运行速度，一次传输多个个数据
        val sData = slave Flow Vec(Bits(config.DATA_STREAM_WIDTH bits), config.WINDOWS_SIZE_H)
        val mData = master Flow Bits(config.DATA_STREAM_WIDTH bits)
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)
        val colNumIn = in UInt (config.SIZE_WIDTH bits)
        val inValid = in Bits (3 bits)
        val mask = in Bits(8 bits)
    }
    //向量与固定的常量相乘，使用加法运算替代。
    def vectorDotProduct(inputData: Vec[UInt], outputData: UInt): Unit = {
        require(inputData.length == 7)
        val temp1 = Vec(UInt(inputData(0).getWidth + 3 bits), 4)
        (0 to 2).foreach((i : Int) => temp1(i) := (inputData(i) +^ inputData(6 - i)).resized)
        temp1(3) := (inputData(3) @@ U"3'b0") - inputData(3)
        val temp2 = Vec(UInt(inputData(0).getWidth + 6 bits), 4)
        temp2(0) := ((temp1(0) @@ U"3'b0").resize(inputData(0).getWidth + 6) + temp1(0)).resized
        temp2(1) := ((temp1(1) @@ U"4'b0").resize(inputData(0).getWidth + 6) + temp1(1)).resized
        temp2(2) := ((temp1(2) @@ U"1'b0").resize(inputData(0).getWidth + 6) + temp1(2)).resized
        temp2(3) := temp1(3).resized
        val temp3 = Vec(UInt(inputData(0).getWidth + 7 bits), 2)
        temp3(0) := temp2(0) +^ temp2(1)
        temp3(1) := temp2(2) @@ U"1'b0" + temp2(3)
        val temp4 = UInt(inputData(0).getWidth + 7 bits)
        temp4 := temp3(0) + (temp3(1) @@ U"2'b0").resize(inputData(0).getWidth + 7)
        outputData := temp4

        temp1.setAsReg()
        temp2.setAsReg()
        temp3.setAsReg()
        //temp4.setAsReg()
    }

    def vectorWindow(inputData: Vec[UInt], outputData: Vec[Vec[UInt]], wen: Bool, sel : Bits, inValid : Bits): Unit = {//输出固定信息。
        val inputDataReg = Vec(Vec(UInt(15 bits).setAsReg() init(0), inputData.length), 3)
        when(wen){
            inputDataReg(2) := inputData
            inputDataReg(1) := inputDataReg(2)
            inputDataReg(0) := inputDataReg(1)
        }
        val temp = Vec(UInt(15 bits), 8 + 6)
        val output = Vec(UInt(15 bits), 8 + 6)
        output.setAsReg()
        //处理输出
        for(i <- 0 to 2){
            temp(i) := inputDataReg(0)(i + 5)
            temp(i + 8 + 3) := inputDataReg(2)(i)
        }
        for (i <- 0 to 7) {
            temp(i + 3) := inputDataReg(1)(i)
        }
        //处理padding
        when(sel === 0){//不进行左右复制
            output := temp
        } elsewhen (sel === 1){//左复制
            (3 until 8 + 6).foreach((i:Int) =>  output(i) := temp(i))
            output(0) := temp(6)
            output(1) := temp(5)
            output(2) := temp(4)
        } elsewhen (sel === 3) {
            switch(inValid){
                output := temp
                is(0){
                    output(12) := temp(10)
                    output(13) := temp(9)
                }
                is(1){
                    output(13) := temp(11)
                }
            }
        } otherwise {//右复制
            switch(inValid){
                output := temp
                for(i <- 0 to 7){
                    is(i){
                        output(i + 4) := temp(i + 2)
                        output(i + 5) := temp(i + 1)
                        output(i + 6) := temp(i + 0)
                    }
                }
            }
        }
        //一共8个数每7个分为一组
        for(i <- 0 to 7; j <- 0 to 6){
            outputData(i)(j) := output(i + j)
        }
    }

    val fsm = new StateMachine { //分为四个状态，空闲，有效，和输出down的数据，END
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val END = new State

        val colCnt = WaCounter(isActive(VALID) && io.sData.fire, io.colNumIn.getWidth, io.colNumIn)
        val rowCnt = WaCounter(isActive(VALID) && io.sData.fire && colCnt.valid, io.rowNumIn.getWidth, io.rowNumIn)

        val sel = Reg(Bits(2 bits)) init(0)
        when(!colCnt.count.orR){
            sel := 2
        } elsewhen (colCnt.count === U(1, colCnt.count.getWidth bits)){
            sel := 1
        } elsewhen (colCnt.valid){
            sel := 3
        }otherwise {
            sel := 0
        }

        //让他在last状态把结果输出
        IDLE
          .whenIsActive {
              when(io.start.rise(False)) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive { //行列都为0
              when(isActive(VALID) && io.sData.fire && colCnt.valid && rowCnt.valid) {
                  goto(END)
              }
          }
        END
          .whenIsActive { //使用一个周期计
              goto(IDLE)
          }
    }
    //第一阶段的计算，计算垂直方向上的向量乘法,延迟共计3个周期
    val Datay = Vec(UInt(8 + 7 bits), config.DATA_NUM)
    val Datax = Vec(UInt(8 + 7 + 7 bits).setAsReg(), config.DATA_NUM)
    for(i <- 0 until  config.DATA_NUM){//延迟3个周期
        val dataLow = Vec((0 until config.WINDOWS_SIZE_H).map((j : Int) => io.sData.payload(j)(i * 8 + 7 downto i * 8).asUInt))
        vectorDotProduct(dataLow, Datay(i))
    }

    //第二阶段的计算，计算水平方向上的向量乘法,首先对数据左右进行反射填充，然后选出需要的数据。最后传给vectorDotProduct模块进行计算。
    //延迟两个个周期，sel在第4个周期给出，
    val outputData = Vec(Vec(UInt(15 bits), config.WINDOWS_SIZE_H), config.DATA_NUM)
    val wen = Delay(fsm.isActive(fsm.VALID) && io.sData.valid || fsm.isActive(fsm.END), 3, init = False)//三个周期之后有效数据到达这里
    vectorWindow(Datay, outputData, wen: Bool, Delay(fsm.sel, 3, init = B(0, fsm.sel.getWidth bits)) : Bits, io.inValid) //那怕只有一个也需要计算所以
    //结果延迟3个周期,共延迟8个周期
    for(i <- 0 to 7){//vectorDotProduct延迟三个周期 Datax延迟一个周期，payload延迟一个周期。
        vectorDotProduct(outputData(i), Datax(i))
        when(Delay(io.mask(i) && !fsm.colCnt.count.orR, 9, init = False)){
            io.mData.payload.subdivideIn(8 slices)(i) := B(0, config.DATA_WIDTH bits)
        } otherwise {
            when(Datax(i)(13)){
                io.mData.payload.subdivideIn(8 slices)(i) := (Datax(i)(8 + 7 + 7 - 1 downto 14) + 1).asBits
            } otherwise{
                io.mData.payload.subdivideIn(8 slices)(i) := Datax(i)(8 + 7 + 7 - 1 downto 14).asBits
            }
        }

    }
    //延迟一个周期一共9个周期。
    io.mData.payload.setAsReg()//增加一个周期
    io.mData.valid := Delay(((fsm.isActive(fsm.VALID) && io.sData.fire && (fsm.colCnt.count.orR || fsm.rowCnt.count.orR))|| fsm.isActive(fsm.END)), 10, init = False) //延迟4个周期输出
}


object GaussianBlur extends App {
    SpinalVerilog(new GaussianBlur(GaussianBlurConfig()))
}