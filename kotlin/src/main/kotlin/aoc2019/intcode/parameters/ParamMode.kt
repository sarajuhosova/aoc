package aoc2019.intcode.parameters

import aoc2019.intcode.State
import aoc2019.intcode.expections.UnknownParamModeException

enum class ParamMode(
    val getLocation: (State, Int) -> Int
) {
    POSITION({ state, offset -> state.read(offset).toInt() }),
    IMMEDIATE({ state, offset -> state.getIP() + offset }),
    RELATION({ state, offset -> state.getBase() + state.read(offset).toInt() });

    companion object {
        private val map = entries.associateBy { it.ordinal }
        operator fun get(index: Int): ParamMode {
            if (index !in map) throw UnknownParamModeException(index)
            return map[index]!!
        }
    }
}
