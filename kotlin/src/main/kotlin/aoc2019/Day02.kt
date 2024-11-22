package aoc2019

import aoc2019.intcode.Computer
import library.Year
import library.readFirst

fun main() {
    val computer = Computer(readFirst(Year._2019, 2))

    // part 1
    println(computer.run(12, 2))

    // part 2
    for (noun in 0..99) {
        for (verb in 0..99) {
            val result = computer.run(noun, verb)
            if (result == 19690720) {
                println(100 * noun + verb)
                break
            }
        }
    }
}
