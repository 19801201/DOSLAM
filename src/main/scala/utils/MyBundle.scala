package utils

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class ImageSize(SIZE_WIDTH:Int) extends Bundle with IMasterSlave {

    val rowNum = UInt (SIZE_WIDTH bits)
    val colNum = UInt (SIZE_WIDTH bits)

    def this(SIZE_WIDTH:Int, colNum : UInt, rowNum:UInt){
        this(SIZE_WIDTH)
        this.rowNum := rowNum.resized
        this.colNum := colNum.resized
    }

    override def asMaster(): Unit = {
        colNum.asOutput()
        rowNum.asOutput()
    }

    def subOne(): ImageSize = {
        sub(1, 1)
    }

    def sub(COLNUM: Int, ROWNUM: Int): ImageSize = {
        new ImageSize(this.SIZE_WIDTH, colNum - COLNUM, rowNum - ROWNUM)
    }

    def setSel(selNum : UInt): ImageSize =  {
        new ImageSize(this.SIZE_WIDTH, (colNum ## selNum).asUInt, rowNum)
    }

    def regNextWhen(wen : Bool) : ImageSize = {
        new ImageSize(this.SIZE_WIDTH, RegNextWhen(colNum, wen, 0), RegNextWhen(rowNum, wen, 0))
    }
}
