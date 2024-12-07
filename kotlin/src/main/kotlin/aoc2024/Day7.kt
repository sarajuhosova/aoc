package aoc2024

import library.Year
import library.readData

fun Long.concat(other: Long): Long = (this.toString() + other.toString()).toLong()

fun checkEquation(target: Long, acc: Long, input: List<Long>, concat: Boolean = true): Boolean {
    if (target < acc) return false
    if (input.isEmpty()) return target == acc
    val current = input.first()
    val rest = input.drop(1)

    return checkEquation(target, acc + current, rest, concat)
            || checkEquation(target, acc * current, rest, concat)
            || (concat && checkEquation(target, acc.concat(current), rest))
}

fun checkEquation(target: Long, input: List<Long>, concat: Boolean = true): Boolean =
    if (input.isEmpty()) false else checkEquation(target, input.first(), input.drop(1), concat)

fun main() {
    val equations = readData(Year._2024, 7)
        .map { it.split(": ") }
        .map { it[0].toLong() to it[1].split(" ").map { i -> i.toLong() } }

    println(equations.filter { checkEquation(it.first, it.second, concat = false) }.sumOf { it.first })
    println(equations.filter { checkEquation(it.first, it.second) }.sumOf { it.first })
}
