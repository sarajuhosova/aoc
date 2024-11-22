package aoc2019

import aoc2019.intcode.Computer
import library.Year
import library.readFirst

fun main() {
    val computer = Computer(readFirst(Year._2019, 5, "e2"))

//    class Part1IO() : DefaultIO() {
//        override fun read(): Int = 1
//    }
//
//    computer.run(io = Part1IO())
//
//    class Part2IO() : DefaultIO() {
//        override fun read(): Int = 5
//    }

    computer.run()

}
