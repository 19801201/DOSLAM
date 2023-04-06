package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

case class WindowsConfig() {
  val DATA_WIDTH = 8
  val DATA_STREAM_WIDTH = DATA_WIDTH * 8
  //输入像素的位宽
  val SIZE_WIDTH = 11
  //输入大小的位宽
  val WINDOWS_SIZE = 2
  //窗口的大小
  val MEM_DEPTH = 1024 //一行图像的个数
  //MEM储存的深度
  val MEM_NUM = WINDOWS_SIZE - 1
  //创建的MEM个数
}
class Windows(windowsConfig : WindowsConfig) extends Component{
  val io = new Bundle {
    val sData = slave Stream (Bits(windowsConfig.DATA_WIDTH bits))
    val mData = master Stream (Vec(Vec(Bits(windowsConfig.DATA_WIDTH bits),windowsConfig.WINDOWS_SIZE), windowsConfig.WINDOWS_SIZE))
    //输入信号和输出信号，确保size*size个数据同时输出
    val start = in Bool()
    //开始信号
    val rowNumIn = in UInt (windowsConfig.SIZE_WIDTH bits)
    val colNumIn = in UInt (windowsConfig.SIZE_WIDTH bits)
    //输入通道大小，
    val rowNumOut = out UInt (windowsConfig.SIZE_WIDTH bits)
    val colNumOut = out UInt (windowsConfig.SIZE_WIDTH bits)
  }
  val valid1 = Reg(Bool()) init false
  //状态机，根据输出来定义状态，所有数据全部输出后进入END状态
  io.rowNumOut := io.rowNumIn - (windowsConfig.WINDOWS_SIZE - 1)
  io.colNumOut := io.colNumIn - (windowsConfig.WINDOWS_SIZE - 1)
  val fsm = new StateMachine {
    setEncoding(binaryOneHot)
    val IDLE = new State with EntryPoint
    val VALID = new State
    val END = new State
    //一共有三个状态IDLE准备状态，接受到start信号上升沿开始工作 进入valid状态
    //VALID 工作状态，只有valid状态才正常工作 当接受完所有数据，并且所有数据都已经输出那么进入END状态
    //END状态，

    //在这里进行输入控制。a::一定是每来一个数据才曾加1
    val colCnt = WaCounter(io.sData.fire, windowsConfig.SIZE_WIDTH, io.colNumIn - 1)
    val rowCnt = WaCounter(io.sData.fire && colCnt.valid, windowsConfig.SIZE_WIDTH, io.rowNumIn - 1)
    //流水线最后一个数据接收到的位置，根据这个位置判断当前计算结果是否满足需求
    val dataOut = Bool()
    val ValidEndT = Reg(Bool()) init False
    val ValidEnd = Reg(Bool()) init False
    when(colCnt.valid && rowCnt.valid && io.sData.fire){
      ValidEndT := True
    }//代表正在接受最后一个数据，下个周期就表示数据已经全部被接收了
    ValidEnd := ValidEndT//拉高时所有数据都已经接收完了。
    //数据全部输出后拉高
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
        when(ValidEnd && !io.mData.valid) { //r：接受完所有数据 并且 每个模块内都没有有效数据。
          goto(END)
        }
      }
    END
      .whenIsActive { //使用一个周期计数
        goto(IDLE)
        ValidEnd := False
      }
  }
  //流水控制模块，因为这一层级内只有两级流水所以很好弄，只使用两级级流水，valid代表寄存器组模块是否有效，ready代表寄存器组模块是否能够接受数据
  //fire代表寄存器组模块成功接受到数据。
  //a:接收到第n-1行，第n-1个数据开始有效，这个数据可以输出第一个窗口
  val dataValidIn = fsm.colCnt.count > (windowsConfig.WINDOWS_SIZE - 2) & fsm.rowCnt.count > (windowsConfig.WINDOWS_SIZE - 2)
  //输出第1个窗口时开始有效，需要等待数据一个周期
  val dataValidOut = RegNext(dataValidIn, io.sData.fire)
  //valid代表读数据模块是否有效，
  val ready0 = (fsm.isActive(fsm.VALID) && !fsm.ValidEndT) && ( !io.mData.valid || io.mData.ready )//这一级模块没有数据还是下一级模块的数据无效都能接收数据。
  //1、满足逻辑要求：当前模块可以接收数据 或者下一级模块可以接收数据。
  //2、满足控制要求：如果所有数据都已经被接收那么就不再接收新的数据。
  when(ready0){//当ready被拉高时数据可以随意进入，valid的值也依赖于上一级模块
    valid1 := io.sData.valid & dataValidIn
  }
  io.sData.ready := ready0
  //a::数据有效并且满足窗口范围内的数据
  io.mData.valid := valid1
  //1、当前处于激活状态，2、当前模块可接受数据 或 下一级模块可接受数据 》》 那么就可以接受上一级模块的数据

  //定义MEM模块，每个MEM用于储存一行的数据，
  val windows = Vec(Vec(Reg(Bits(windowsConfig.DATA_WIDTH bits)) init 0, windowsConfig.WINDOWS_SIZE ), windowsConfig.WINDOWS_SIZE)
  //定义地址模块，每次接收到一个数据地址加1，同时存到MEM和
  val rdAddr = fsm.colCnt.count //周期1：读取数据
  val wrAddr = Reg(UInt(windowsConfig.SIZE_WIDTH bits)) init(0)
  when(io.sData.fire){
    wrAddr := rdAddr
  }
  val rdData = Vec(Bits(windowsConfig.DATA_WIDTH bits), windowsConfig.WINDOWS_SIZE)//读取的数据缓存一个周期
  val wrData = Vec(Bits(windowsConfig.DATA_WIDTH bits), windowsConfig.WINDOWS_SIZE - 1)
  for(i <- 0 until windowsConfig.WINDOWS_SIZE - 1){//写数据
    wrData(i) := windows(i + 1)(0)
  }//将上一个MEM读取的数据存入到下一个模块
  rdData(windowsConfig.WINDOWS_SIZE - 1) := io.sData.payload //读数据
  val mem = Array.tabulate(windowsConfig.MEM_NUM)(i => {
    def gen(): Mem[Bits] = {
      val mem = Mem(Bits(windowsConfig.DATA_WIDTH bits), wordCount = windowsConfig.MEM_DEPTH).addAttribute("ram_style = \"block\"")
      mem.write(wrAddr.resized, wrData(i), io.sData.fire)
      //存入
      rdData(i) := mem.readAsync(rdAddr.resized)
      //从0到n-2的范围的MEM读出
      mem
    }

    gen()
  })
  //windows模块，定义储存器。fire满足时不断向右驱动。
  for(h <- 0 until  windowsConfig.WINDOWS_SIZE){
    for(w <- 0 until  windowsConfig.WINDOWS_SIZE){
      io.mData.payload(h)(windowsConfig.WINDOWS_SIZE - 1 - w) := windows(h)(w) //给下一级模块赋值
      if(w == 0){
        when(io.sData.fire){
          windows(h)(w) := rdData(h)
        }
      } else {
        when(io.sData.fire) {
          windows(h)(w) := windows(h)(w - 1)
        }
      }
    }
  }
}

object Windows extends App {
  SpinalVerilog(new Windows(WindowsConfig()))
}
