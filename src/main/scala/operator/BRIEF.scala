package operator

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
import wa.ip._
import spinal.lib.experimental.chisel.Module

import scala.io.Source

case class BRIEFConfig(DATA_NUM : Int = 31,
                          MEM_DEPTH : Int = 1024,
                          SIZE_WIDTH : Int = 11
                         ) {
    val WINDOWS_SIZE_H = 7
    val DATA_WIDTH = 8
    val DATA_STREAM_WIDTH = DATA_WIDTH * DATA_NUM
    val computeNum = 32
    val currentDir: String = System.getProperty("user.dir")
    println(currentDir)
    //F:\Project\SpinalHDLProject\SpinalHDLOrbSlamAccelerator
    val filename: String = "F:\\Project\\SpinalHDLProject\\SpinalHDLOrbSlamAccelerator\\src\\main\\data\\Coordinate.dat"
    val separator: String = ","
    val lines: Seq[String] = Source.fromFile(filename).getLines.toSeq
    val array: Array[Array[Int]] = lines.map(_.split(separator).map(_.trim.toInt)).toArray
//    printf("%d\n",array.length);//打印查看结果是否正确
//    for(temp <- array){
//        for(a <- temp){
//            printf("%d ",a);
//        }
//        println(" ")
//    }
}

class BRIEF(config:BRIEFConfig) extends Module{
    val io = new Bundle {//要求valid有效之后数据需要保持8周期的有效值//
        val sData = slave Flow Vec(Vec(UInt(config.DATA_WIDTH bits), config.DATA_NUM), config.DATA_NUM) //第一个是得分点,其余的是周围的点
        val mData = master Flow Bits(config.computeNum bits)//传出M01和M10两个数据
    }

    def control(sel: UInt, first: Bool, end: Bool): Unit = { //整个模块的控制模块
        //现在的valid是不对的需要延迟若干个周期与 乘法器配合
        val timeCount = WaCounter(io.sData.valid, 3, 7) //从0记录到7，必须保证valid信号有效的时间是8个周期，让timeCount能够恢复初始状态
        sel := timeCount.count
        end := timeCount.valid
        first := timeCount.count.orR
    }

    def compare(dataA : Vec[UInt], dataB : Vec[UInt]) : Bits = {//计算一次描述子的值
        val res = Bits(config.computeNum bits)
        for(i <- 0 until config.computeNum){
            res(i) := dataA(i) < dataB(i)
        }
        res
    }

    def dataSel(isA : Boolean, sel: UInt): Vec[UInt]={
        val selData = Vec(UInt(config.DATA_WIDTH bits), 32)
        var add = 0
        if(isA){
            add = 2
        }
        switch(sel){
            for(i <- 0 to 7){
                is(i){
                    for(j <- 0 until config.computeNum){
                        val x = 15 + config.array(i * config.computeNum + j)(0 + add)
                        val y = 15 + config.array(i * config.computeNum + j)(1 + add)
                        selData(j) := io.sData.payload(y)(x)
                    }
                }
            }
        }
        selData
    }

    val sum = new Area {
        val sel = UInt(3 bits)
        val first,end = Bool
        control(sel, first, end)

        val dataA = dataSel(false, sel: UInt)
        val dataB = dataSel(true, sel: UInt)

        io.mData.payload := compare(dataA, dataB)
        io.mData.valid := Delay(io.sData.valid, 1)
        io.mData.payload.setAsReg() init(0)
    }
}


object BRIEF extends App {
    SpinalVerilog(new BRIEF(BRIEFConfig()))
    //val a = BRIEFConfig()
}