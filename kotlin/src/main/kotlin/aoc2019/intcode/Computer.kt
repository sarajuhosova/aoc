package aoc2019.intcode

import aoc2019.intcode.io.DefaultIO
import aoc2019.intcode.io.IO

data class Computer(
    private val input: String
) {
    private val memory: Memory = Memory(input)

    private var last: Memory = memory.copy()
    fun readResult(index: Int = 0): Long = last[index]

    suspend fun run(noun: Long, verb: Long) {
        run(mapOf(1 to noun, 2 to verb))
    }

    suspend fun run(
        overwrite: Map<Int, Long> = mapOf(),
        io: IO = DefaultIO()
    ) {
        last = memory.copy()
        for ((key, value) in overwrite) {
            last[key] = value
        }
        State(last, io).execute()
    }

}
