package aoc2019.intcode

enum class Instruction(
    val id: Int,
    val arguments: Int,
    val execute: (State) -> Unit
) {
    ADD(
        1,
        3,
        { state -> state.updateLoc(state.readLoc(1) + state.readLoc(2), 3) }
    ),
    MUL(
        2,
        3,
        { state -> state.updateLoc(state.readLoc(1) * state.readLoc(2), 3) }
    ),
    END(
        99,
        3,
        { state -> state.halt() }
    );

    companion object {
        private val idToInstruction: Map<Int, Instruction> = values().associateBy { it.id }
        fun getById(id: Int): Instruction =
            if (id in idToInstruction) idToInstruction[id]!!
            else throw UnknownInstructionException()
    }
}
