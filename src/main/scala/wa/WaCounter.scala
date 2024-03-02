package wa

import spinal.core.{Area, Bool, False, IntToBuilder, Reg, True, U, UInt, when}
import spinal.lib.Counter

object WaCounter {
  def apply(en: Bool, width: Int, end: UInt) = new WaCounter(en, width, end, 0)
  def apply(en: Bool, width: Int, end: UInt, InitData: Int) = new WaCounter(en, width, end, InitData)
  def apply(en: Bool, width: Int) = new WaCounter(en, width, U((1 << width) - 1, width bits), 0)
}

class WaCounter(en: Bool, width: Int, cnt: UInt, InitData: Int) extends Area {
  val count = Reg(UInt(width bits)) init InitData
  val valid = Bool()
  when(count === cnt) {
    valid := True
  } otherwise {
    valid := False
  }
  when(en) {
    count := count + 1
    when(valid) {
      count := 0
    }
  }

  def clear = {
    count := InitData
    valid := False
  }

  def validLast() = {
    valid && en
  }
}

class BootCount(wen:Bool, WIDTH:Int, endCount:UInt){
  val count = Reg(UInt(WIDTH bits))
  val last = count === endCount
  val boot = (Reg(Bool()) init(False) clearWhen(last) setWhen(wen)) || wen
  when(boot){
    count := count + 1
    when(last){
      count := 0
    }
  }
}