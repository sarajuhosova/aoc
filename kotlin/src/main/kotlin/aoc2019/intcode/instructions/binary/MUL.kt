package aoc2019.intcode.instructions.binary

import aoc2019.intcode.State
import aoc2019.intcode.instructions.Instruction3
import aoc2019.intcode.parameters.Param

object MUL: Instruction3 {

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override fun State.exec(lhs: Param, rhs: Param, target: Param): Boolean {
        update(read(lhs) * read(rhs), target)
        return false
    }

}
