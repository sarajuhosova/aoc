package aoc2024.day03

import library.Year
import library.parseRegex
import library.readData

val MUL_REGEX = Regex("mul[(]([0-9]*),([0-9]*)[)]")
val ALL_REGEX = Regex("mul[(][0-9]*,[0-9]*[)]|do[(][)]|don't[(][)]")

fun get(mul: String): Long {
    val matched = mul.parseRegex(MUL_REGEX)
    return matched[0].toLong() * matched[1].toLong()
}

fun multiply(instructions: List<String>, ignore: Boolean = false): Long {
    var result = 0L
    var doo = true
    for (instruction in instructions) {
        if (instruction == "do()") doo = true
        else if (instruction == "don't()") doo = ignore || false
        else if (doo) result += get(instruction)
    }
    return result
}

fun main() {
    val instructions = ALL_REGEX.findAll(readData(Year._2024, 3)
        .joinToString(""))
        .map { it.value }
        .toList()

    println(multiply(instructions, true))
    println(multiply(instructions))
}
