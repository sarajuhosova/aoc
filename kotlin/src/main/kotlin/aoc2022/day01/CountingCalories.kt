package aoc2022.day01

import library.*

fun sumMaxes(list: List<Long>, amount: Int): Long =
    when (amount) {
        0 -> 0L
        else -> {
            val max = list.max()
            max + sumMaxes(list.removeFirst(max), amount - 1)
        }
    }

fun main() {
    println("Hello, Advent of Code 2022!")

    val data = parseAsGrouped(
        readData(Year._2022, "day01.txt"),
        ""
    ) { d -> d.map { it.toLong() }.sumOf() }

    println(data.max())
    println(sumMaxes(data, 3))
}
