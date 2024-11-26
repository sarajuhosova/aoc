package aoc2019.intcode

import aoc2019.intcode.io.DefaultIO
import aoc2019.intcode.io.IO

data class Computer(
    private val input: String
) {
    private val memory = input.split(",")
        .map { it.toInt() }.toTypedArray()

    private var last: Memory = memory.copy()
    fun readResult(index: Int = 0): Int = last[index]

    suspend fun run(
        noun: Int = memory[1],
        verb: Int = memory[2],
        io: IO = DefaultIO()
    ) {
        last = memory.copy()
        last[1] = noun
        last[2] = verb
        State(last, io).execute()
    }

}
