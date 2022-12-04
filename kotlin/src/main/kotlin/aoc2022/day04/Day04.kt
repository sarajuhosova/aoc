package aoc2022.day04

import library.Year
import library.readData

fun parse(line: String): Pair<Pair<Int, Int>, Pair<Int, Int>> {
    val data = Regex("([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)")
        .find(line)!!.destructured.toList()
        .map { it.toInt() }

    return Pair(Pair(data[0], data[1]), Pair(data[2], data[3]))
}

fun compare(pair: Pair<Pair<Int, Int>, Pair<Int, Int>>): Boolean =
    (pair.first.first <= pair.second.first && pair.second.second <= pair.first.second)
            || (pair.second.first <= pair.first.first && pair.first.second <= pair.second.second)


fun overlap(pair: Pair<Pair<Int, Int>, Pair<Int, Int>>): Boolean =
    (pair.first.first <= pair.second.first && pair.first.second >= pair.second.first)
            || (pair.second.first <= pair.first.first && pair.second.second >= pair.first.first)

fun main() {
    val pairs = readData(Year._2022, "day04.txt").map { parse(it) }

    println(pairs.count{ compare(it) })
    println(pairs.count{ overlap(it) })
}
