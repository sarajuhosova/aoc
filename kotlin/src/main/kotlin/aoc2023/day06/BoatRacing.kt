package aoc2023.day06

import library.Year
import library.longProduct
import library.readData
import java.util.*

fun countRecords(time: Int, current: Int): Int =
    (0..time).map { it * (time - it) }
        .count { it > current }

fun bruteForce(races: List<Pair<Int, Int>>): Long =
    races.map { countRecords(it.first, it.second) }.longProduct()

fun lowerBound(race: Long, record: Long): Long {
    var lower = 0L
    var distance = 0L
    while (distance < record) {
        lower++
        distance = lower * (race - lower)
    }
    return lower
}

fun upperBound(race: Long, record: Long): Long {
    var upper = race
    var distance = 0L
    while (distance < record) {
        upper--
        distance = upper * (race - upper)
    }
    return upper
}


fun findBounds(race: Long, record: Long): Long =
    (upperBound(race, record) - lowerBound(race, record)) + 1

fun parse(line: String): List<Int> {
    val sc = Scanner(line.substring(line.indexOf(":") + 1))

    val result = mutableListOf<Int>()
    while (sc.hasNextInt()) result.add(sc.nextInt())
    return result
}

fun parse(lines: List<String>): Pair<List<Int>, List<Int>> =
    Pair(parse(lines[0]), parse(lines[1]))

fun unkern(list: List<Int>): Long =
    list.joinToString("") { it.toString() }.toLong()

fun main() {
    println("Hello, Advent of Code!")

    val (ms, records) = parse(readData(Year._2023, 6))

    println(bruteForce(ms.zip(records)))
    println(findBounds(unkern(ms), unkern(records)))
}
