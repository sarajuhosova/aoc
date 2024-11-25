package aoc2019.intcode.instructions.control

import aoc2019.intcode.State
import aoc2019.intcode.instructions.Instruction2
import aoc2019.intcode.parameters.Param

object JUMP_IF_TRUE: Instruction2 {

    @Suppress("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
    override fun State.exec(condition: Param, destination: Param): Boolean =
        if (read(condition) != 0) {
            setPointer(read(destination))
            true
        } else false

}
