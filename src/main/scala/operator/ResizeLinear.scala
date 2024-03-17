package operator

import spinal.core.Component.push
import wa.ip._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
//使用固定缩放1.25倍开始计算，
case class ResizeConfig() {
    val DATA_WIDTH = 8//输入像素的位宽
    val DATA_SIZE = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * 8 //输入数据的总数
    val SIZE_WIDTH = 11  //输入大小的位宽
    val WINDOWS_SIZE = 2  //窗口的大小
    val MEM_DEPTH = 1024 //一行图像的个数/8
    //初始化数据
    val initF = Array(1, 3, 5, 7, 0) //所有数据的初值
    val initSelFx = Array(0, 1, 2, 3, 5, 6, 7, 8)
}
class ResizeLinear(resizeConfig : ResizeConfig) extends Component{

    val io = new Bundle {
        //增加运行速度，一次传输8个数据
        val mData = master Stream Bits(resizeConfig.DATA_STREAM_WIDTH bits)
        val sData = slave Stream Vec(Vec(Bits(resizeConfig.DATA_WIDTH bits), 10), 2)
        //输入信号和输出信号，确保size*size个数据同时输出
        val start = in Bool()
        //开始信号
        val rowNumSrcIn = in UInt (resizeConfig.SIZE_WIDTH bits) //窗口的输入大小。输入数据的个数不管到底有多少
        val colNumSrcIn = in UInt (resizeConfig.SIZE_WIDTH bits) //输出数据的大小由上位机给出即可，完全不考虑边界问题。即输入即输出数据量

        val mask = in Bits(8 bits)
    }
    //1、状态机模块
    val fsm = new StateMachine {
      setEncoding(binaryOneHot)
      val IDLE = new State with EntryPoint
      val VALID = new State
      val END = new State
      //一共有三个状态IDLE准备状态，接受到start信号上升沿开始工作 进入valid状态
      //VALID 工作状态，只有valid状态才正常工作 当接受完所有数据，并且所有数据都已经输出那么进入END状态
      //END状态，

      //在这里进行输入控制。a::一定是每来一个数据才曾加1
      //一次输入八个数据这样可以更快的计算，


      val colCnt = WaCounter(io.sData.fire, resizeConfig.SIZE_WIDTH - 3, io.colNumSrcIn)//从最大值开始
      val rowCnt = WaCounter(io.sData.fire && colCnt.valid, resizeConfig.SIZE_WIDTH, io.rowNumSrcIn)
      val rowCntValid = WaCounter(io.sData.fire && colCnt.valid, resizeConfig.SIZE_WIDTH, 4)
      //流水线最后一个数据接收到的位置，根据这个位置判断当前计算结果是否满足需求
      //状态跳转
      IDLE
        .whenIsActive {
          when(io.start.rise()) {
            goto(VALID)
          }
        }
      //colCnt.count >= U(maxPoolingFixConfig.kernelSize - 1, log2Up(maxPoolingFixConfig.kernelSize - 1) bits)
      VALID
        .whenIsActive { //行列都为0.并且模块内部数据都
          when(colCnt.valid && rowCnt.valid && io.sData.fire) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
            goto(END)
          }
        }
      END
        .whenIsActive { //使用一个周期计数
          goto(IDLE)
        }
    }
    //2、处理权重的状态------------------1级流水

    val fy = Reg(UInt(3 bits)) //每次变化一个值每行结束就会变化。
    val dfy = Delay(fy,2)
    fy.init(resizeConfig.initF(0))//每次移动一个单位
    when(io.sData.fire && fsm.colCnt.valid && fsm.rowCnt.valid) { //数据来临的时候需要换到下一个权重，如果在行末，需要重置权重，如果没有数据来临需要保持权重不变
        fy := U(resizeConfig.initF(0), 3 bits)
        fsm.rowCntValid.clear
    } elsewhen (io.sData.fire && fsm.colCnt.valid) {//到行末，修改值
        switch(fy(2 downto 0)) {
            for (j <- 0 to 4) {
                is(resizeConfig.initF((j))) {
                    fy := resizeConfig.initF((j + 1) % 5)
                }
            }
            default{
                fy := 0
            }
        }
    }
    //3、数据的选择模块 ------------------
    //根据selFx来选择输入数据,将输入数据存到这里一个周期,后续只做一个11位的加法，时序不太严格,但是这里的数据选择很麻烦，因此还是采用一级流水吧。
    //dataSel和selFx需要选择同一级流水。selFx拥有初始值，直接递增即可。权重处理也需要和这里选择同一级流水，这样可以同时得到两个所需参数
    val dataSel = Vec(Vec(UInt(resizeConfig.DATA_WIDTH bits),4), 8)//数据寄存一个周期，沉余一半的点，可以删除
    for(i <- 0 to 7) {//假设数据的加载方式是 按地址顺序来的
        dataSel(i)(0) := io.sData.payload(0)(resizeConfig.initSelFx(i)).asUInt
        dataSel(i)(1) := io.sData.payload(0)(resizeConfig.initSelFx(i) + 1).asUInt
        dataSel(i)(2) := io.sData.payload(1)(resizeConfig.initSelFx(i)).asUInt
        dataSel(i)(3) := io.sData.payload(1)(resizeConfig.initSelFx(i) + 1).asUInt
    }
    //4、计算的模块------------------5级流水，需要经过五个加法器
    //1.数据乘以x的相对距离,变为加法操作，一共有5种情况
    //fx的乘法和加法计算，
    val dataFxMul = Vec(Vec(Reg(UInt(11 bits)), 4), 8)
    val dataFxMulSum = Vec(Vec(Reg(UInt(11 bits)), 2), 8)
    val dataFyMul = Vec(Vec(Reg(UInt(14 bits)), 2), 8)
    val dataFyMulSum = Vec(Reg(UInt(10 bits)), 8)
    //1--------------使用加法代替乘法,每次权重都固定不变,写死
    for(i <- 0 to 1){//1,7,3,5,5,3,7,1
        dataFxMul(i * 4)(0) := (dataSel(i * 4)(0) << 3) - dataSel(i * 4)(0)
        dataFxMul(i * 4)(1) := dataSel(i * 4)(1).resized
        dataFxMul(i * 4)(2) := (dataSel(i * 4)(2) << 3) - dataSel(i * 4)(2)
        dataFxMul(i * 4)(3) := dataSel(i * 4)(3).resized

        dataFxMul(i * 4 + 1)(0) := (dataSel(i * 4 + 1)(0) +^ (dataSel(i * 4 + 1)(0) << 2)).resized
        dataFxMul(i * 4 + 1)(1) := (dataSel(i * 4 + 1)(1) +^ (dataSel(i * 4 + 1)(1) << 1)).resized
        dataFxMul(i * 4 + 1)(2) := (dataSel(i * 4 + 1)(2) +^ (dataSel(i * 4 + 1)(2) << 2)).resized
        dataFxMul(i * 4 + 1)(3) := (dataSel(i * 4 + 1)(3) +^ (dataSel(i * 4 + 1)(3) << 1)).resized

        dataFxMul(i * 4 + 2)(0) := (dataSel(i * 4 + 2)(0) +^ (dataSel(i * 4 + 2)(0) << 1)).resized
        dataFxMul(i * 4 + 2)(1) := (dataSel(i * 4 + 2)(1) +^ (dataSel(i * 4 + 2)(1) << 2)).resized
        dataFxMul(i * 4 + 2)(2) := (dataSel(i * 4 + 2)(2) +^ (dataSel(i * 4 + 2)(2) << 1)).resized
        dataFxMul(i * 4 + 2)(3) := (dataSel(i * 4 + 2)(3) +^ (dataSel(i * 4 + 2)(3) << 2)).resized

        dataFxMul(i * 4 + 3)(0) :=  dataSel(i * 4 + 3)(0).resized
        dataFxMul(i * 4 + 3)(1) :=  (dataSel(i * 4 + 3)(1) << 3) - dataSel(i * 4 + 3)(1)
        dataFxMul(i * 4 + 3)(2) :=  dataSel(i * 4 + 3)(2).resized
        dataFxMul(i * 4 + 3)(3) :=  (dataSel(i * 4 + 3)(3) << 3) - dataSel(i * 4 + 3)(3)
    }
    //2--------------对计算结果求和
    for(i <- 0 to 7){
        dataFxMulSum(i)(0) := dataFxMul(i)(0) + dataFxMul(i)(1)
        dataFxMulSum(i)(1) := dataFxMul(i)(2) + dataFxMul(i)(3)
    }
    //3-4--------------对计算结果求y的乘法,
    val CalculateY = Array.tabulate(8)((i) => {
        def gen() = {
            //在这里定义加减法器
            val opNum1 = Vec(UInt(14 bits), 2)
            val opNum2 = Vec(UInt(11 bits), 2)
            val res = Vec(Bits(14 bits), 2)
            val op = Vec(Bool(), 2)
            dataFyMul(i)(0) := res(0).asUInt
            dataFyMul(i)(1) := res(1).asUInt

            val addSub0 = AddSub(14, 11, 14, AddSubConfig.unsigned, AddSubConfig.unsigned, 1, AddSubConfig.lut, this.clockDomain, AddSubConfig.addSubtract, "fyMul", i == 0)
            addSub0.io.A := opNum1(0)
            addSub0.io.B := opNum2(0)
            addSub0.io.ADD <> op(0)
            res(0) := addSub0.io.S.asBits
            val addSub1 = AddSub(14, 11, 14, AddSubConfig.unsigned, AddSubConfig.unsigned, 1, AddSubConfig.lut, this.clockDomain, AddSubConfig.addSubtract, "fyMul", false)
            addSub1.io.A := opNum1(1)
            addSub1.io.B := opNum2(1)
            addSub1.io.ADD <> op(1)
            res(1) := addSub1.io.S.asBits


            //利用加法代替乘法
            opNum2(0) := dataFxMulSum(i)(0)
            opNum2(1) := dataFxMulSum(i)(1)
            op(0) := True
            op(1) := True
            switch(dfy(2 downto 1)) {//参数选择
                is(3) { //不变和8-1 1 7
                    opNum1(0) := U"14'b0"
                    opNum1(1) := (dataFxMulSum(i)(1) @@ U"3'b0").resized
                    op(1) := False
                }
                is(2) { //2+1 和4+1 3 5
                    opNum1(0) := (dataFxMulSum(i)(0) @@ U"1'b0").resized
                    opNum1(1) := (dataFxMulSum(i)(1) @@ U"2'b0").resized
                }
                is(1) { //4+1 and 4+1 5 3
                    opNum1(0) := (dataFxMulSum(i)(0) @@ U"2'b0").resized
                    opNum1(1) := (dataFxMulSum(i)(1) @@ U"1'b0").resized
                }
                is(0) { //and 8-1 and 7 + 1
                    opNum1(0) := (dataFxMulSum(i)(0) @@ U"3'b0").resized
                    opNum1(1) := U"14'b0"
                    op(0) := False
                }
            }
        }

        gen()
    })
    //5---------------求最后的结果
    for(i <- 0 to 7){
        dataFyMulSum(i) := (dataFyMul(i)(0)(13 downto 4) + dataFyMul(i)(1)(13 downto 4) + 2)
    }

    //5、流水线控制模块,
    //首先要防止数据丢失，使用FIFO和valid状态延迟，将输入有效信号延迟若干个周期
    val fifo = StreamFifo(Bits(resizeConfig.DATA_STREAM_WIDTH bits), 20)
    val sReady = Bool()
    when(fifo.io.availability < U"4'd8") { //当还可以储存的数据量较少时设置sReady为低，输入不在正常工作，valid值正常向后传输
        sReady := False
    } otherwise {
        sReady := True
    }
    io.sData.ready := sReady && fsm.isActive(fsm.VALID)
    for(i <- 0 to 7){
        when(Delay(io.mask(i) && fsm.colCnt.valid, 5)) {
            fifo.io.push.payload.subdivideIn(resizeConfig.DATA_SIZE slices)(i) := 0
        } otherwise {
            fifo.io.push.payload.subdivideIn(resizeConfig.DATA_SIZE slices)(i) := dataFyMulSum(i).asBits(9 downto 2)
        }

    }
    fifo.io.push.valid <> Delay(io.sData.fire && !fsm.rowCntValid.valid, 5)//给定的数据延迟若干个周期
    fifo.io.pop <> io.mData//得到的结果直接传递给外部
}

object ResizeLinear extends App {
    SpinalVerilog(new ResizeLinear(ResizeConfig()))
}