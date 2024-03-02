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

/**
 * io接口
 * start0 开启输入数据
 * start1 开启计算
 *
 * last0 输入结束
 * last1 一个特征值的输入结束
 * last2 重新进入空闲
 *
 * 状态机
 * IDLE：不接收数据，等等开始指令
 * INPUT：接收数据这些数据是，整体存储用于特征匹配的
 * COMPUTEINPUT：接收1组数据用于特征匹配
 * COMPUTE：计算特征匹配
 */

object fsmIC {
    def apply[T <: Data](start:Vec[Bool], last:Vec[Bool])  = {
        new StateMachine {
            setEncoding(binaryOneHot)
            val IDLE = new State with EntryPoint
            val INPUT = new State
            val COMPUTEINPUT = new State
            val COMPUTE = new State
            val READY = new State

            IDLE
              .whenIsActive {
                  when(start(0)) {
                      goto(INPUT)
                  } elsewhen(start(1)){
                      goto(COMPUTEINPUT)
                  }
              }
            INPUT
              .whenIsActive {
                  when(last(0)) { //图像接收完毕并且窗口 整个图像都已经被传输走
                      goto(IDLE)
                  }
              }
            COMPUTEINPUT
              .whenIsActive {
                  when(last(1)) { //图像接收完毕并且窗口 整个图像都已经被传输走
                      goto(IDLE)
                  } elsewhen last(0) {
                      goto(COMPUTE)
                  }
              }
            COMPUTE
              .whenIsActive {
                  when(last(2)) { //图像接收完毕并且窗口 整个图像都已经被传输走
                      goto(READY)
                  }
              }
            READY
              .whenIsActive {
                  when(last(3)){
                    goto(COMPUTEINPUT)
                  }
              }
        }
    }
}

