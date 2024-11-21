package aoc2019

import library.Year
import library.readInts

fun part1(data: List<Int>): Int = data.sumOf { (it / 3) - 2 }

fun part2(data: List<Int>): Int {
    return 0
}

fun main() {
    println("Hello, Advent of Code!")

    val data = readInts(Year._2019, 1)

    println(part1(data))
    println(part2(data))
}
