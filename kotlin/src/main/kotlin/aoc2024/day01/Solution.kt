package aoc2024.day01

import library.Year
import library.readData
import kotlin.math.abs

fun calculateDifferences(data: List<Pair<Int, Int>>): Int =
    data.sumOf { abs(it.second - it.first) }

fun calculateSimilarity(left: List<Int>, right: List<Int>): Int =
    left.sumOf { l -> right.count { r -> r == l } * l }

fun main() {
    println("Hello, Advent of Code!")

    val data = readData(Year._2024, 1)
        .map { it.split("   ") }
        .map { it[0].toInt() to it[1].toInt() }

    val left = data.map { it.first }.sorted()
    val right = data.map { it.second }.sorted()

    println(calculateDifferences(left.zip(right)))
    println(calculateSimilarity(left, right))
}
