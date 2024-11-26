package aoc2019.intcode.instructions

import aoc2019.intcode.State
import aoc2019.intcode.parameters.Param

object HALT: Instruction0 {

    override suspend fun State.exec(): Boolean {
        halt()
        return true
    }

}
