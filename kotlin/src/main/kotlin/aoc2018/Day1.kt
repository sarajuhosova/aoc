package aoc2018

import library.Year
import library.parseRegex
import library.readData

typealias Change = (Int) -> Int

fun findFinalFrequency(changes: List<Change>): Int =
    changes.fold(0) { acc, f -> f(acc) }

fun findRepeatingFrequency(changes: List<Change>): Int {
    val frequencies = mutableSetOf<Int>()

    var frequency = 0
    var i = 0
    while (true) {
        frequency = changes[i](frequency)
        if (frequencies.contains(frequency)) return frequency
        frequencies.add(frequency)
        i = (i + 1) % changes.size
    }
}

fun main() {
    println("Hello, Advent of Code!")

    val changes: List<Change> = readData(Year._2018, 1)
        .parseRegex("([+|-])([0-9]+)") { s -> {
            i -> if (s[0] == "+") i + s[1].toInt() else i - s[1].toInt()
        } }

    println(findFinalFrequency(changes))
    println(findRepeatingFrequency(changes))
}
