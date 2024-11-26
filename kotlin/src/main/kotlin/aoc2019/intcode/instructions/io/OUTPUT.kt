package aoc2019.intcode.instructions.io

import aoc2019.intcode.State
import aoc2019.intcode.instructions.Instruction1
import aoc2019.intcode.parameters.Param

object OUTPUT: Instruction1 {

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override suspend fun State.exec(source: Param): Boolean {
        output(read(source))
        return false
    }

}