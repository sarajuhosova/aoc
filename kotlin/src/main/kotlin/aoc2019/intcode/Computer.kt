package aoc2019.intcode

import aoc2019.intcode.io.DefaultIO
import aoc2019.intcode.io.IO

data class Computer(
    private val input: String
) {
    private val memory: Memory = Memory(input)

    private var last: Memory = memory.copy()
    fun readResult(index: Int = 0): Long = last[index]

    suspend fun run(
        noun: Long = memory[1],
        verb: Long = memory[2],
        io: IO = DefaultIO()
    ) {
        last = memory.copy()
        last[1] = noun
        last[2] = verb
        State(last, io).execute()
    }

}
