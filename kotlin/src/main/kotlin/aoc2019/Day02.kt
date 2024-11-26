package aoc2019

import aoc2019.intcode.Computer
import kotlinx.coroutines.runBlocking
import library.Year
import library.readFirst

fun main() {
    runBlocking {
        val computer = Computer(readFirst(Year._2019, 2))

        // part 1
        computer.run(12, 2)
        println(computer.readResult())

        // part 2
        for (noun in 0..99) {
            for (verb in 0..99) {
                computer.run(noun, verb)
                val result = computer.readResult()
                if (result == 19690720) {
                    println(100 * noun + verb)
                    break
                }
            }
        }
    }
}
