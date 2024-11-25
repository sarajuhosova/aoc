package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.DefaultIO
import library.Year
import library.readFirst

fun main() {
    val computer = Computer(readFirst(Year._2019, 5))

    class Part1IO() : DefaultIO() {
        override fun read(): Int = 1
    }

    computer.run(io = Part1IO())

    class Part2IO() : DefaultIO() {
        override fun read(): Int = 5
    }

    computer.run(io = Part2IO())
}
