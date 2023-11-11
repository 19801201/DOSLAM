package utils

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import wa.WaCounter

object fsmIV {
    def apply[T <: Data](start:Bool, sData : Stream[T], size : ImageSize) = {
        new StateMachine {
            setEncoding(binaryOneHot)
            val IDLE = new State with EntryPoint
            val VALID = new State

            val validCnt = ImageCount(sData.fire, size)

            IDLE
              .whenIsActive {
                  when(start.rise()) {
                      goto(VALID)
                  }
              }
            VALID
              .whenIsActive {
                  when(validCnt.fireCnt) { //图像接收完毕并且窗口 整个图像都已经被传输走
                      goto(IDLE)
                  }
              }
        }
    }
}
//接受subOne的size数据
object fsmIVPRF {
    def apply[T <: Data](start:Bool, wens : Bool, wenm : Bool, sizeIn : ImageSize, sizeOut : ImageSize, PADDING_NUM : Int) = {
        new StateMachine {
            setEncoding(binaryOneHot)
            val IDLE = new State with EntryPoint
            val VALID = new State
            val PADDING = new State
            val READY = new State
            val FLUSH = new State

            val readyCnt = ImageCount(wenm, sizeOut)
            val validCnt = ImageCount(wens && isActive(VALID), sizeIn)
            val paddingCnt = ImageCount(wens && isActive(PADDING), sizeIn.colNum, U(PADDING_NUM - 1, sizeIn.colNum.getWidth bits))

            IDLE
              .whenIsActive {
                  when(start.rise()) {
                      goto(VALID)
                  }
              }
            VALID
              .whenIsActive {
                  when(validCnt.fireCnt) { //图像接收完毕并且窗口 整个图像都已经被传输走
                      goto(PADDING)
                  }
              }
            PADDING
              .whenIsActive {
                  when(paddingCnt.fireCnt){
                      goto(READY)
                  }
              }
            READY.whenIsActive{
                when(readyCnt.fireCnt){
                    goto(FLUSH)
                }
            }
            FLUSH.whenIsActive{
                goto(IDLE)
            }
        }
    }
}
