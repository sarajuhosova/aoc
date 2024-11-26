package aoc2019.intcode.instructions.binary

import aoc2019.intcode.State
import aoc2019.intcode.instructions.Instruction3
import aoc2019.intcode.parameters.Param

object EQUAL_TO: Instruction3 {

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override suspend fun State.exec(lhs: Param, rhs: Param, target: Param): Boolean {
        update(if (read(lhs) == read(rhs)) 1 else 0, target)
        return false
    }

}
