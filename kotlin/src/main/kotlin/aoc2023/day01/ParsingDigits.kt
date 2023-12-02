package aoc2023.day01

import library.Year
import library.readData

val WORD_TO_NUM = mapOf(
    Pair("one", 1),
    Pair("two", 2),
    Pair("three", 3),
    Pair("four", 4),
    Pair("five", 5),
    Pair("six", 6),
    Pair("seven", 7),
    Pair("eight", 8),
    Pair("nine", 9)
)

fun searchForDigits(data: List<String>): Int =
    data.map {
        it.filter { c -> c in '0'..'9' }
    }.sumOf { s -> (s[0] - '0') * 10 + (s[s.length - 1] - '0') }

fun checkForDigit(s: String): Int? {
    // check whether the first number is an actual digit
    if (s[0] in '0'..'9') return (s[0] - '0')
    // otherwise check for a word
    val key = WORD_TO_NUM.keys.find { s.startsWith(it) }
    return if (key != null) WORD_TO_NUM[key]!! else null
}

fun searchForAllDigits(data: List<String>): Int {
    var sum = 0

    for (line in data) {
        for (i in line.indices) {
            val maybe = checkForDigit(line.substring(i))
            if (maybe != null) {
                sum += maybe * 10
                break
            }
        }

        for (i in (line.length - 1) downTo 0) {
            val maybe = checkForDigit(line.substring(i))
            if (maybe != null) {
                sum += maybe
                break
            }
        }
    }

    return sum
}

fun main() {
    println("Hello, Advent of Code 2023!")

    val data = readData(Year._2023, 1)

    println(searchForDigits(data))
    println(searchForAllDigits(data))
}
