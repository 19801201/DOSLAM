package data

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter


case class BitwidthConversionConfig() {
    val DATA_WIDTH = 8
    val DATA_NUM_IN = 8
    val DATA_NUM_OUT = 10
    val DATA_STREAM_WIDTH_IN = DATA_WIDTH * DATA_NUM_IN
    val DATA_STREAM_WIDTH_OUT = DATA_WIDTH * DATA_NUM_OUT
    //输入像素的位宽
    val SIZE_WIDTH = 11
    //输入大小的位宽
    val WINDOWS_SIZE = 2
    //窗口的大小
    val MEM_DEPTH = 1024 //一行图像的个数/8
    //MEM储存的深度
    val MEM_NUM = WINDOWS_SIZE - 1
    //创建的MEM个数
    val VALID_COL_WIDTH = 3
}
//一次传输64bit 需要变为 80bit
/*
    第一个数据储存，第2，3，4，5个数据传输80bit，需要特判末尾数据是否有效
 */
/*
    本模块将8个8位数据，拼接位10个8位数据，一个周期出结果
 */


class BitwidthConversion(config: BitwidthConversionConfig) extends Component {//现在不支持 无效数据
    val io = new Bundle {
        val sData = slave Stream Bits(config.DATA_STREAM_WIDTH_IN bits)
        val mData = master Stream Bits(config.DATA_STREAM_WIDTH_OUT bits)
        val start = in Bool()
        val rowNumIn = in UInt (config.SIZE_WIDTH bits)//输入行数为rowNumIn+1
        val colNumIn = in UInt (config.SIZE_WIDTH - 3 bits)//输入列数为colNumIn + 1,每次包含八列数字，最后一列可能包含无效数据
//        val colValidNumIn = in UInt (3 bits)//最后一列的有效数据个数为i+1
    }
    //状态机 状态机的跳转
    val fsm = new StateMachine {
        setEncoding(binaryOneHot)
        val IDLE = new State with EntryPoint
        val VALID = new State
        val ENDLINE = new State
        val END = new State
        //一共有四个状态IDLE准备状态，接受到start信号上升沿开始工作 进入valid状态
        //VALID 工作状态，只有valid状态才正常工作 当接受完所有数据，并且所有数据都已经输出那么进入END状态
        //END状态，
        //在这里进行输入控制。a::一定是每来一个数据才曾加1
        //一次输入八个数据这样可以更快的计算，
        val colCnt = WaCounter(io.sData.fire, config.SIZE_WIDTH - 3, io.colNumIn) //从最大值开始
        val rowCnt = WaCounter(io.sData.fire && colCnt.valid, config.SIZE_WIDTH, io.rowNumIn)
        //流水线最后一个数据接收到的位置，根据这个位置判断当前计算结果是否满足需求
        val dataInvalid = Bool()
        val EndOfLine = io.mData.fire || dataInvalid
        //状态跳转
        IDLE
          .whenIsActive {
              when(io.start.rise()) {
                  goto(VALID)
              }
          }
        VALID
          .whenIsActive { //行列都为0.并且模块内部数据都
              when(colCnt.valid && io.sData.fire) {
                  goto(ENDLINE)
              }
          }
        ENDLINE //如果是最后一行，那么处理完进入END状态，如果不是最后一行处理完进入VALID状态，传输下一个数据
          .whenIsActive {
              when(EndOfLine) {//剩余的数据被下级模块接收 、、 或者剩余的数据都不是无效数据
                  when(rowCnt.count.orR === False) {//最后一行跳转到end状态
                      goto(END)
                  } otherwise {
                      goto(VALID)
                  }
                  colCnt.clear
              }
          }
        END
          .whenIsActive { //使用一个周期计数
              goto(IDLE)
          }
    }
    //位宽转换模块，将8bit位宽的数据转换为10bit的位宽数据
    val sReady = Reg(Bool()) init False
    val BWCcount = WaCounter(io.sData.fire, 3, 4) //0 1 2 3 4，每次加1，其代表存下来的有效数据量是多少，0-》1的时候存8bit，4-》0的时候将8bit和暂存的数据全部传出
    // sDataPayTemp = Bits(config.DATA_STREAM_WIDTH_IN bits)
    //判断末尾有效数据位数
    fsm.dataInvalid := !BWCcount.count.orR
    /*
    0 : 0
    1 : 8  0到7位还没有被接收
    2 : 6  2到7位还没有被接收
    3 : 4  4到7位还没有被接收
    4 : 2  6到7位还没有被接收
     */
    val mValid = io.sData.valid || (fsm.isActive(fsm.ENDLINE) && !fsm.dataInvalid)
      //复位
    when(fsm.EndOfLine && fsm.isActive(fsm.ENDLINE)) { //到了行末需要重新清零数据，
        BWCcount.clear
    }
    //衔接--上层模块
    val sDataPayTemp = RegNextWhen(io.sData.payload, io.sData.fire, B"64'b0") //接收上层数据
    io.sData.ready := fsm.isActive(fsm.VALID) && (io.mData.ready || !io.mData.valid)
    //1、valid状态 2、下层模块ready拉高时拉高，或者没有有效数据 3、最后一个数据的时候下层模块ready信号与上层ready信号隔离。
    //衔接--下层模块

    switch(BWCcount.count) { //fire的时候，sdata,边界条件如何处理？？？，
        io.mData.valid := False
        io.mData.payload := B"80'b0"
        is(0) { //BWCcount.count代表接收了BWCcount.count个数据，这里还没有接收数据    64
            io.mData.valid := False
            io.mData.payload.asBits := B"80'b0"
        }
        is(1) { //已经接收了一个数据，加上sData给的可以拼成一个数据，8
            io.mData.payload := io.sData.payload(15 downto 0) ## sDataPayTemp
            io.mData.valid := mValid
            //1、上层valid拉高 2、这里存在最后的有效数据没有传递->有效数据中的无效数据->有效数据中的有数据
        }
        is(2) { //已经接收了两个数据，加上sData给的可以拼成一个数据，
            io.mData.payload :=  io.sData.payload(31 downto 0) ## sDataPayTemp.asBits(63 downto 16)
            io.mData.valid := mValid
        }
        is(3) {
            io.mData.payload := io.sData.payload(47 downto 0) ## sDataPayTemp.asBits(63 downto 32)
            io.mData.valid := mValid
        }
        is(4) { //已经接收了四个数据，加上sData给的可以拼成一个数据，sData的数据变为废数，不在需要，2
            io.mData.payload := io.sData.payload(63 downto 0) ## sDataPayTemp.asBits(63 downto 48)
            io.mData.valid := mValid
        }
    }
}

object BitwidthConversion extends App {
    SpinalVerilog(new BitwidthConversion(BitwidthConversionConfig()))
}
