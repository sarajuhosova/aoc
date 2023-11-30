package aoc2020.day03

import library.Year
import library.product
import library.readData

fun countTrees(data: List<String>, jump: Int): Long = data
    .mapIndexed { index, line -> line[(index * jump) % line.length] }
    .count { it == '#' }.toLong()

fun countTrees(data: List<String>, jump: Int, skip: Int): Long = countTrees(
    if (skip > 1) data.filterIndexed { index, _ -> index % skip == 0 } else data,
    jump
)

fun evaluatePaths(data: List<String>): Long =
    arrayOf(Pair(1, 1), Pair(3, 1), Pair(5, 1), Pair(7, 1), Pair(1, 2))
        .map { countTrees(data, it.first, it.second) }
        .product()

fun main() {
    println("Hello, Advent of Code!")

    val data = readData(Year._2020, 3)

    println(countTrees(data, 3))
    println(evaluatePaths(data))
}
