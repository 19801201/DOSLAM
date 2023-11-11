package data

import spinal.core.{U, _}
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter
//生成窗口的大小

class BaseDataGenerateConfig( val WSIZE: Int = 3,
                              val HSIZE: Int = 3,
                              val MEM_DEPTH : Int = 1024
                            ){
    val MEM_NUM = HSIZE - 1
}
//无反压的数据转换
class BaseDataGenerate[T <: Data](hardType: HardType[T] ,config:BaseDataGenerateConfig) {
    //
    def rowBuf(rdData: Vec[T], inputData: T, wen: Bool, EndAddr: UInt): Unit = {
        val count = WaCounter(wen, log2Up(config.MEM_DEPTH), EndAddr)
        val rdAddr = count.count //周期1：读取数据
        val wrAddr = RegNext(rdAddr, U(0))
        val mem = Array.tabulate(config.MEM_NUM)(i => {
            def gen(): Mem[T] = {
                val mem = Mem(hardType, wordCount = config.MEM_DEPTH).addAttribute("ram_style = \"block\"")

                mem.write(wrAddr, rdData(i + 1), RegNext(wen)) //同步写,使能延迟
                //存入
                rdData(i) := mem.readSync(rdAddr) //同步读
                //从0到n-2的范围的MEM读出
                mem
            }

            gen()
        })
        rdData(config.MEM_NUM) := RegNext(inputData)
    }

    def reversal(input: Vec[SInt], output: Vec[SInt]): Unit = {
        require(input.size == output.size)
        for (i <- 0 until input.size) {
            output(i) := input(input.size - 1 - i)
        }
    }

    def colBuf(rdData: Vec[SInt], input: Flow[SInt]) {
        for (i <- 0 until config.WSIZE) {
            if (i == 0) rdData(i) := input.payload
            else rdData(i) := RegNextWhen(rdData(i - 1), input.valid)
        }
    }


}
