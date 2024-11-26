package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.DefaultIO
import kotlinx.coroutines.runBlocking
import library.Year
import library.readFirst

fun main() = runBlocking {
    val computer = Computer(readFirst(Year._2019, 9))

    class Day09AIO: DefaultIO() {
        override suspend fun provide(): Long = 1
    }

    computer.run(io = Day09AIO())

    class Day09BIO: DefaultIO() {
        override suspend fun provide(): Long = 2
    }

    computer.run(io = Day09BIO())
}
