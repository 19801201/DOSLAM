package data
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import wa.WaCounter
/**
 * 生成一个缓存MEM_NUM行数据的buf，每输入一个数据，下个周期读出垂直的n行数据，
 * inputData
 */
object RowBuf {
    def apply[T<:Data](inputData: Flow[T], EndAddr: UInt, MEM_DEPTH : Int, WINDOWS_SIZE_H : Int):Vec[T] = {
        val count = WaCounter(inputData.fire, log2Up(MEM_DEPTH), EndAddr)
        val rdAddr = count.count //周期1：读取数据
        val wrAddr = RegNext(rdAddr, U(0))
        val rdData = Vec(HardType(inputData.payload), WINDOWS_SIZE_H)
        val mem = Array.tabulate(WINDOWS_SIZE_H - 1)(i => {
            def gen(): Mem[T] = {
                val mem = Mem(HardType(inputData.payload), wordCount = MEM_DEPTH).addAttribute("ram_style = \"block\"")

                mem.write(wrAddr, rdData(i + 1), RegNext(inputData.fire)) //同步写,使能延迟
                //存入
                rdData(i) := mem.readSync(rdAddr, inputData.fire) //同步读
                //从0到n-2的范围的MEM读出
                mem
            }

            gen()
        })
        rdData(WINDOWS_SIZE_H - 1) := RegNextWhen(inputData.payload, inputData.fire)
        rdData
    }
}
/**
 * 缓存一个窗口数据WINDOWS_SIZE_W * rdData.size大小的窗口，当wen使能时
 */
object WindowsBuf{
    def apply[T<:Data](rdData: Flow[Vec[T]], WINDOWS_SIZE_W:Int): Vec[Vec[T]] = {
        val windows = Vec(Vec(weakCloneOf(rdData.payload.head), WINDOWS_SIZE_W), rdData.payload.size)
        for (h <- 0 until rdData.payload.size) {
            for (w <- 0 until WINDOWS_SIZE_W) {
                if (w == WINDOWS_SIZE_W - 1) {
                    windows(h)(w) := rdData.payload(h)
                } else {
                    windows(h)(w).setAsReg()
                    when(rdData.fire) {
                        windows(h)(w) := windows(h)(w + 1)
                    }
                }
            }
        }
        windows
    }
}

/**
 * 对称映射一个数据
 */
object reversal{
    def apply[T <: Data](input: Vec[T], output: Vec[T]): Unit = {
        for (i <- input.indices) {
            output(i) := input(input.size - 1 - i)
        }
    }
}

object dataFlip{

    def apply[T <: Data](input: Vec[T], output: Vec[T], sel: UInt, Flip: Array[(UInt, Array[Int])]): Unit = {
        switch(sel) { //左测翻转1
            for (p <- Flip) {
                val (key, pos) = p
                is(key) {
                    for (i <- 0 until output.length) {
                        output(i) := input(pos(i))
                    }
                }
                default {
                    output := input
                }
            }
        }
    }
}


@DontName
object BORDER_REFLECT{
    val FlipDefault = U"5'b11111"

    val Flip5 = Array(
        U"5'b00000" -> Array(1, 1, 2, 3, 4),
        U"5'b00001" -> Array(3, 2, 2, 3, 4),
        U"5'b00010" -> Array(0, 1, 2, 2, 1),
        U"5'b00011" -> Array(0, 1, 2, 3, 3),
        FlipDefault -> Array(0, 1, 2, 3, 4))

    val Flip11 = Array(
        U"5'b00000" -> Array(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        U"5'b00001" -> Array(3, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        U"5'b00010" -> Array(5, 4, 3, 3, 4, 5, 6, 7, 8, 9, 10),
        U"5'b00011" -> Array(7, 6, 5, 4, 4, 5, 6, 7, 8, 9, 10),
        U"5'b00100" -> Array(9, 8, 7, 6, 5, 5, 6, 7, 8, 9, 10),
        U"5'b00101" -> Array(0, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
        U"5'b00110" -> Array(0, 1, 2, 3, 4, 5, 6, 6, 5, 4, 3),
        U"5'b00111" -> Array(0, 1, 2, 3, 4, 5, 6, 7, 7, 6, 5),
        U"5'b01000" -> Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 7),
        U"5'b01001" -> Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9),
        FlipDefault -> Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    val Flip17 = Array(
        U"5'b00000" -> Array( 1,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00001" -> Array( 3,  2,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00010" -> Array( 5,  4,  3,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00011" -> Array( 7,  6,  5,  4,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00100" -> Array( 9,  8,  7,  6,  5,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00101" -> Array(11, 10,  9,  8,  7,  6,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00110" -> Array(13, 12, 11, 10,  9,  8,  7,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b00111" -> Array(15, 14, 13, 12, 11, 10,  9,  8,  8,  9, 10, 11, 12, 13, 14, 15, 16),
        U"5'b01000" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  7,  6,  5,  4,  3,  2,  1),
        U"5'b01001" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  9,  8,  7,  6,  5,  4,  3),
        U"5'b01010" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 10,  9,  8,  7,  6,  5),
        U"5'b01011" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 11, 10,  9,  8,  7),
        U"5'b01100" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 12, 11, 10,  9),
        U"5'b01101" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 13, 12, 11),
        U"5'b01110" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 14, 13),
        U"5'b01111" -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 15),
        FlipDefault -> Array( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16))
}

@DontName
object BORDER_REFLECT101{
    val FlipDefault = U"5'b11111"

    val Flip3 = Array(
        U"5'b00000" -> Array(0, 1, 0),
        U"5'b00001" -> Array(2, 1, 2),
        FlipDefault -> Array(0, 1, 2))

    val Flip5 = Array(
        U"5'b00000" -> Array(0, 1, 2, 3, 2),
        U"5'b00001" -> Array(0, 1, 2, 1, 0),
        U"5'b00010" -> Array(4, 3, 2, 3, 4),
        U"5'b00011" -> Array(2, 1, 2, 3, 4),
        FlipDefault -> Array(0, 1, 2, 3, 4))

    val Flip7 = Array(
        U"5'b00000" -> Array(0, 1, 2, 3, 4, 5, 4),
        U"5'b00001" -> Array(0, 1, 2, 3, 4, 3, 2),
        U"5'b00010" -> Array(0, 1, 2, 3, 2, 1, 0),
        U"5'b00011" -> Array(6, 5, 4, 3, 4, 5, 6),
        U"5'b00100" -> Array(4, 3, 2, 3, 4, 5, 6),
        U"5'b00101" -> Array(2, 1, 2, 3, 4, 5, 6),
        FlipDefault -> Array(0, 1, 2, 3, 4, 5, 6))

    def getFlip(size: Int) = {
        size match {
            case 3 => Flip3
            case 5 => Flip5
            case 7 => Flip7
        }
    }
}
object formatConversion{
    def apply(windows: Vec[Vec[Bits]], DATA_NUM:Int): Vec[Vec[Vec[Bits]]] = {
        val DATA_WIDTH = windows.head.head.getWidth / DATA_NUM
        val WINDOWS_W = windows.head.size
        val WINDOWS_H = windows.size
        val temp = Vec(Vec(Vec(Bits(DATA_WIDTH bits), WINDOWS_W), WINDOWS_H), DATA_NUM)

        for (i <- 0 until DATA_NUM) {
            for (j <- 0 until WINDOWS_H) {
                for (k <- 0 until WINDOWS_W) {
                    temp(i)(j)(k) := windows(j).asBits.subdivideIn(WINDOWS_W * DATA_NUM slices)(7 + i + k)
                }
            }
        }

        temp
    }
}

//@DontName
//object MaskKey{
//
//
//    //def apply() = MASK
//
//}
