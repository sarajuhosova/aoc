package aoc2019.intcode.instructions

import aoc2019.intcode.State
import aoc2019.intcode.expections.UnknownInstructionException
import aoc2019.intcode.instructions.binary.ADD
import aoc2019.intcode.instructions.binary.EQUAL_TO
import aoc2019.intcode.instructions.binary.LESS_THAN
import aoc2019.intcode.instructions.binary.MUL
import aoc2019.intcode.instructions.control.JUMP_IF_FALSE
import aoc2019.intcode.instructions.control.JUMP_IF_TRUE
import aoc2019.intcode.instructions.io.INPUT
import aoc2019.intcode.instructions.io.OUTPUT
import aoc2019.intcode.parameters.Param

interface Instruction {
    val arguments: Int

    suspend fun execute(state: State, params: Array<Param>) {
        // run the instruction
        val jumped = exec(state, params)
        // update the pointer
        if (!jumped) state.movePointer(arguments + 1)
    }

    suspend fun exec(state: State, params: Array<Param>): Boolean

    companion object {
        fun get(opcode: Int): Instruction = when (opcode) {
            1 -> ADD
            2 -> MUL
            3 -> INPUT
            4 -> OUTPUT
            5 -> JUMP_IF_TRUE
            6 -> JUMP_IF_FALSE
            7 -> LESS_THAN
            8 -> EQUAL_TO
            9 -> ADJUST_BASE
            99 -> HALT
            else -> throw UnknownInstructionException(opcode)
        }
    }
}

interface Instruction0: Instruction {
    override val arguments: Int get() = 0

    override suspend fun exec(state: State, params: Array<Param>): Boolean =
        state.exec()

    suspend fun State.exec(): Boolean
}

interface Instruction1: Instruction {
    override val arguments: Int get() = 1

    override suspend fun exec(state: State, params: Array<Param>): Boolean =
        state.exec(params[0])

    suspend fun State.exec(arg1: Param): Boolean
}

interface Instruction2: Instruction {
    override val arguments: Int get() = 2

    override suspend fun exec(state: State, params: Array<Param>): Boolean =
        state.exec(params[0], params[1])

    suspend fun State.exec(arg1: Param, arg2: Param): Boolean
}

interface Instruction3: Instruction {
    override val arguments: Int get() = 3

    override suspend fun exec(state: State, params: Array<Param>): Boolean =
        state.exec(params[0], params[1], params[2])

    suspend fun State.exec(arg1: Param, arg2: Param, arg3: Param): Boolean
}
