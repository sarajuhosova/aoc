package aoc2019.intcode.instructions

import aoc2019.intcode.Memory

data class Param(
    val mode: ParamMode,
    val offset: Int
) {
    fun getLocation(memory: Memory, ip: Int) : Int {
        return mode.getLocation(memory, ip + offset)
    }
}
