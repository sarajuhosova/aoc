package aoc2019.intcode.instructions

import aoc2019.intcode.State
import aoc2019.intcode.parameters.Param

object ADJUST_BASE: Instruction1 {

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override suspend fun State.exec(offset: Param): Boolean {
        moveBase(read(offset))
        return false
    }

}
