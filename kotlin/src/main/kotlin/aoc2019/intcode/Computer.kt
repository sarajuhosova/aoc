package aoc2019.intcode

data class Computer(
    private val input: String
) {
    private val memory = input.split(",")
        .map { it.toInt() }.toTypedArray()

    fun run(noun: Int = memory[1], verb: Int = memory[2]): Int {
        val mem = memory.copy()
        mem[1] = noun
        mem[2] = verb
        val state = State(mem)
        while (true) {
            val instruction = Instruction.getById(state.read())
            instruction.execute(state)
            state.movePointer(instruction.arguments + 1)
            if (state.isHalted()) return mem[0]
        }
    }

}
