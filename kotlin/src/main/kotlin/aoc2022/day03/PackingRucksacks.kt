package aoc2022.day03

import library.Year
import library.readData

fun convert(char: Char): Int =
    if (char >= 'a') (char - 'a' + 1) else (char - 'A' + 27)

fun parse(data:List<String>): List<List<Int>> =
    data.map { string -> string.map { convert(it) } }

fun findDuplicates(rucksacks: List<List<Int>>): List<Int> =
    rucksacks
        .map { Pair(it.take(it.size / 2), it.drop(it.size / 2)) }
        .map { it.second.find { item -> it.first.contains(item) } !! }

fun findCommon(rucksacks: List<List<Int>>): Int =
    rucksacks[0]
        .filter { rucksacks[1].contains(it) }
        .find { rucksacks[2].contains(it) } !!

fun findBadges(rucksacks: List<List<Int>>): Int =
    when (rucksacks.size) {
        0 -> 0
        else -> findCommon(rucksacks.take(3)) + findBadges(rucksacks.drop(3))
    }

fun main() {
    val rucksacks = parse(readData(Year._2022, "day03.txt"))

    println(findDuplicates(rucksacks).sum())
    println(findBadges(rucksacks))
}
