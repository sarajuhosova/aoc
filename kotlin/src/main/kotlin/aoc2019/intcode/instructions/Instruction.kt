package aoc2019.intcode.instructions

import aoc2019.intcode.State
import aoc2019.intcode.expections.UnknownInstructionException

enum class Instruction(
    val id: Int,
    val arguments: Int,
    val action: (State, Array<Param>) -> Boolean
) {
    // Adds together numbers read from two positions
    //  and stores the result in a third position.
    ADD(
        1,
        3,
        { state, params ->
                state.update(
                    state.read(params[0]) + state.read(params[1]),
                    params[2]
                )
                false
        }
    ),
    // Multiplies together numbers read from two positions
    //  and stores the result in a third position.
    MUL(
        2,
        3,
        { state, params ->
            state.update(
                state.read(params[0]) * state.read(params[1]),
                params[2]
            )
            false
        }
    ),
    // Takes a single integer as input and saves it to the position given by its only parameter.
    // For example, the instruction 3,50 would take an input value and store it at address 50.
    INPUT(
        3,
        1,
        { state, params ->
            state.update(state.input(), params[0])
            false
        }
    ),
    // Outputs the value of its only parameter.
    // For example, the instruction 4,50 would output the value at address 50.
    OUTPUT(
        4,
        1,
        { state, params ->
            state.output(state.read(params[0]))
            false
        }
    ),
    // If the first parameter is non-zero,
    //  it sets the instruction pointer to the value from the second parameter.
    // Otherwise, it does nothing.
    JUMP_IF_TRUE(
        5,
        2,
        { state, params ->
            if (state.read(params[0]) != 0) {
                state.setPointer(state.read(params[1]))
                true
            } else false
        }
    ),
    // If the first parameter is zero,
    //  it sets the instruction pointer to the value from the second parameter.
    // Otherwise, it does nothing.
    JUMP_IF_FALSE(
        6,
        2,
        { state, params ->
            if (state.read(params[0]) == 0) {
                state.setPointer(state.read(params[1]))
                true
            } else false
        }
    ),
    // If the first parameter is less than the second parameter,
    //  it stores 1 in the position given by the third parameter.
    // Otherwise, it stores 0.
    LESS_THAN(
        7,
        3,
        { state, params ->
            state.update(if (state.read(params[0]) < state.read(params[1])) 1 else 0, params[2])
            false
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
            false
        }
    ),
    // The program is finished and should immediately halt.
    HALT(
        99,
        0,
        { state, _ ->
            state.halt()
            true
        }
    );

    companion object {
        private val idToInstruction: Map<Int, Instruction> = values().associateBy { it.id }
        fun getById(id: Int): Instruction =
            if (id in idToInstruction) idToInstruction[id]!!
            else throw UnknownInstructionException(id)
    }
}
