package aoc2021.day1

import library.Year
import library.readInts
import library.tail

fun countIncreases(data: List<Int>): Int =
    data.zip(data.tail()).count { it.first < it.second }

fun slide(data: List<Int>): Int =
    countIncreases(
        data.zip(data.tail().zip(data.tail().tail()))
            .map { it.first + it.second.first + it.second.second }
    )

fun main() {
    val data = readInts(Year._2021, "day01.txt")

    println(countIncreases(data))
    println(slide(data))
}