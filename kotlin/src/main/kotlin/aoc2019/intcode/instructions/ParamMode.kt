package aoc2019.intcode.instructions

import aoc2019.intcode.Memory
import aoc2019.intcode.expections.UnknownParamModeException

enum class ParamMode(
    val getLocation: (Memory, Int) -> Int
) {
    POSITION({ memory, index -> memory[index] }),
    IMMEDIATE({ _, index -> index });

    companion object {
        private val map = values().associateBy { it.ordinal }
        operator fun get(index: Int): ParamMode {
            if (index !in map) throw UnknownParamModeException(index)
            return map[index]!!
        }
    }
}
