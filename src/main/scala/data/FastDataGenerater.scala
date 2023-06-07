//package data
//
//import spinal.core._
//import spinal.lib._
//case class FastDataGeneraterConfig(
//                                    DATA_WIDTH: Int = 8,
//                                    DATA_NUM : Int = 8,
//                                    MEM_DEPTH : Int = 1024,
//                                    MME_NUM : Int = 3 + 32 + 3 + 32,
//                                    SIZE_WIDTH : Int = 11
//                                  ){
//
//}
//
//class FastDataGenerater(config : FastDataGeneraterConfig) {
//    val io = new Bundle {
//        //增加运行速度，一次传输8个数据
//        val mData = master Stream Vec(Bits(config.DATA_WIDTH bits), 8)
//        val sData = slave Stream Vec(Bits(config.DATA_NUM bits), config.DATA_NUM)
//        //输入信号和输出信号，确保size*size个数据同时输出
//        val start = in Bool()
//        //开始信号
//        val rowNumSrcIn = in UInt (resizeConfig.SIZE_WIDTH bits) //窗口的输入大小。输入数据的个数不管到底有多少
//        val colNumSrcIn = in UInt (resizeConfig.SIZE_WIDTH bits) //输出数据的大小由上位机给出即可，完全不考虑边界问题。即输入即输出数据量
//    }
//}