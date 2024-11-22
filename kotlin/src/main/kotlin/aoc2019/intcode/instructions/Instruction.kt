package aoc2019.intcode.instructions

import aoc2019.intcode.State
import aoc2019.intcode.expections.UnknownInstructionException
import library.pow

enum class Instruction(
    val id: Int,
    val arguments: Int,
    private val action: (State, Array<Param>) -> Unit,
    val jump: Boolean = false
) {
    // Adds together numbers read from two positions
    //  and stores the result in a third position.
    ADD(
        1,
        3,
        { state, params -> state.update(
            state.read(params[0]) + state.read(params[1]),
            params[2]
        ) }
    ),
    // Multiplies together numbers read from two positions
    //  and stores the result in a third position.
    MUL(
        2,
        3,
        { state, params -> state.update(
            state.read(params[0]) * state.read(params[1]),
            params[2]
        ) }
    ),
    // Takes a single integer as input and saves it to the position given by its only parameter.
    // For example, the instruction 3,50 would take an input value and store it at address 50.
    INPUT(
        3,
        1,
        { state, params -> state.update(state.input(), params[0]) }
    ),
    // Outputs the value of its only parameter.
    // For example, the instruction 4,50 would output the value at address 50.
    OUTPUT(
        4,
        1,
        { state, params -> state.output(state.read(params[0])) }
    ),
    // If the first parameter is non-zero,
    //  it sets the instruction pointer to the value from the second parameter.
    // Otherwise, it does nothing.
    JUMP_IF_TRUE(
        5,
        2,
        { state, params -> if (state.read(params[0]) != 0) state.setPointer(state.read(1)) },
        jump = true
    ),
    // If the first parameter is zero,
    //  it sets the instruction pointer to the value from the second parameter.
    // Otherwise, it does nothing.
    JUMP_IF_FALSE(
        6,
        2,
        { state, params -> if (state.read(params[0]) == 0) state.setPointer(state.read(1)) },
        jump = true
    ),
    // If the first parameter is less than the second parameter,
    //  it stores 1 in the position given by the third parameter.
    // Otherwise, it stores 0.
    LESS_THAN(
        7,
        3,
        { state, params ->
            state.update(if (state.read(params[0]) < state.read(params[1])) 1 else 0, params[2])
        }
    ),
    // If the first parameter is equal to the second parameter,
    //  it stores 1 in the position given by the third parameter.
    // Otherwise, it stores 0.
    EQUALS(
        8,
        3,
        { state, params ->
            state.update(if (state.read(params[0]) == state.read(params[1])) 1 else 0, params[2])
        }
    ),
    // The program is finished and should immediately halt.
    END(
        99,
        3,
        { state, _ -> state.halt() }
    );

    companion object {
        private val idToInstruction: Map<Int, Instruction> = values().associateBy { it.id }
        private fun getById(id: Int): Instruction =
            if (id in idToInstruction) idToInstruction[id]!!
            else throw UnknownInstructionException(id)

        fun execute(state: State) {
            val info = state.read()

            // get the instruction
            val opcode = info % 100
            val instruction = getById(opcode)

            // resolve the parameters
            val params = Array(instruction.arguments) { i ->
                val position = pow(10, i + 2)
                Param(ParamMode[(info / position) % 10], i + 1)
            }

            // run the instruction
            instruction.action(state, params)

            // update the pointer
            if (!instruction.jump) state.movePointer(instruction.arguments + 1)
        }

    }
}
