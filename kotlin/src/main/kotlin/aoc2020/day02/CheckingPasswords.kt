package aoc2020.day02

import library.Year
import library.parseRegex
import library.readData

data class Password(
    val input: List<String>
) {
    private val min: Int = input[0].toInt()
    private val max: Int = input[1].toInt()

    private val char: Char = input[2][0]

    private val pwd: String = input[3]

    fun rangeValid(): Boolean = pwd.count { it == char } in min..max
    fun dataValid(): Boolean = (pwd[min - 1] == char) xor (pwd[max - 1] == char)
}

fun main() {
    val data = readData(Year._2020, 2)
        .parseRegex("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")
        .map { Password(it) }

    println(data.count { it.rangeValid() })
    println(data.count { it.dataValid() })
}
