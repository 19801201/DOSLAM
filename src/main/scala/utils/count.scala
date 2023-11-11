package utils
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

object ImageCount{
    def apply(wen : Bool, size : ImageSize) = new ImageCount(wen, size)

    def apply(wen : Bool, colIn:UInt, rowIn:UInt) =
        new ImageCount(wen, new ImageSize(colIn.getWidth, colIn, rowIn))
}

class ImageCount[T <: Data](wen:Bool, size : ImageSize) extends Area{
    val colCnt = WaCounter(wen, size.colNum.getWidth, size.colNum)
    val rowCnt = WaCounter(wen && colCnt.valid, size.rowNum.getWidth, size.rowNum)
    val fireCnt = wen && colCnt.valid && rowCnt.valid

    def clear(wen : Bool): Unit = {
        when(wen){
            colCnt.clear
            rowCnt.clear
        }
    }

    def getSize(): ImageSize = {
        new ImageSize(size.colNum.getWidth, colCnt.count, rowCnt.count)
    }
}
