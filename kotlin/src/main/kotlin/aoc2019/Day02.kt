package aoc2019

import library.Year
import library.readFirst

typealias Program = Array<Int>

fun Program.copy(): Array<Int> = this.map { it }.toTypedArray()

fun Program.run() {
    var counter = 0
    while (true) {
        when (this[counter]) {
            99 -> break
            1 -> {
                this[this[counter + 3]] = this[this[counter + 1]] + this[this[counter + 2]]
                counter += 4
            }
            2 -> {
                this[this[counter + 3]] = this[this[counter + 1]] * this[this[counter + 2]]
                counter += 4
            }
        }
    }
}

fun Program.run(noun: Int, verb: Int) {
    this[1] = noun
    this[2] = verb
    this.run()
}

fun main() {
    val program: Program = readFirst(Year._2019, 2)
        .split(",").map { it.toInt() }
        .toTypedArray()

    // part 1
    val first = program.copy()
    first.run(12, 2)
    println(first[0])

    // part 2
    for (noun in 0..99) {
        for (verb in 0..99) {
            val copy = program.copy()
            copy.run(noun, verb)
            if (copy[0] == 19690720) {
                println(100 * noun + verb)
                break
            }
        }
    }
}
