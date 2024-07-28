package config

import spinal.core._

object Config {
    val filePath = "./verilog"
    val TbDataPath = "F:\\TestData\\OpencvData"

    val DEBUG_MODULE = true
    val DEBUG_TOUCH = true
    val DEBUG_KEEP_HIERARCHY = true
    def addDebug[a <: Data](temp : a) = if(DEBUG_MODULE) temp.addAttribute("mark_debug", "true")
    def addTouch[a <: Data](temp : a) = if(DEBUG_TOUCH) temp.addAttribute("DONT_TOUCH", "TRUE")
    def addKeepHierarchy[a <: Data](temp : a) = if(DEBUG_KEEP_HIERARCHY) temp.addAttribute("KEEP_HIERARCHY", "TURE")
}
