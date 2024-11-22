package aoc2019.intcode

import aoc2019.intcode.instructions.Instruction
import aoc2019.intcode.io.DefaultIO
import aoc2019.intcode.io.IO

data class Computer(
    private val input: String
) {
    private val memory = input.split(",")
        .map { it.toInt() }.toTypedArray()

    private var last: Memory = memory.copy()
    fun readResult(): Int = last[0]

    fun run(
        noun: Int = memory[1],
        verb: Int = memory[2],
        io: IO = DefaultIO()
    ) {
        last = memory.copy()
        last[1] = noun
        last[2] = verb
        val state = State(last, io)
        while (true) {
            Instruction.execute(state)
            if (state.isHalted()) return
        }
    }

}
