package aoc2020.day01

import library.Year
import library.parseInts
import library.readData

fun double(data: List<Int>): Int = data
    .find { i -> data.any { j -> i + j == 2020 } }
    ?.let { it * (2020 - it) }!!

fun triple(data: List<Int>): Int {
    for (i in data) {
        for (j in data) {
            for (k in data) {
                if (i + j + k == 2020) {
                    return i * j * k
                }
            }
        }
    }
    return 0
}

fun main() {
    println("Hello, Advent of Code!")

    val data = readData(Year._2020, 1).parseInts()

    println(double(data))
    println(triple(data))
}
