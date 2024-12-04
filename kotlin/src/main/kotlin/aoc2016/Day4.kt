package aoc2016

import library.Year
import library.readData

fun shift(c: Char, shift: Int): Char {
    if (c == '-') return ' '
    val id = c - 'a'
    return 'a' + ((id + shift) % 26)
}

data class Room(
    private val id: String,
    val sector: Int
) {
    private val checksum = id.toCharArray()
        .asSequence()
        .filter { c -> c != '-' }
        .distinct()
        .map { c -> c to id.toCharArray().count { c == it } }
        .sortedWith(compareByDescending<Pair<Char, Int>> { it.second }.thenBy { it.first })
        .take(5)
        .map { it.first }
        .joinToString("")

    fun check(sum: String) = sum == checksum

    val name: String = id.toCharArray().map { shift(it, sector) }.joinToString("")
}

fun parse(input: String): Pair<Room, String> {
    val dash = input.lastIndexOf('-')
    val bracket = input.indexOf('[')
    return Pair(
        Room(input.substring(0, dash), input.substring(dash + 1, bracket).toInt()),
        input.substring(bracket + 1, input.length - 1)
    )
}

fun main() {
    val rooms = readData(Year._2016, 4)
        .map { parse(it) }
        .filter { it.first.check(it.second) }
        .map { it.first }

    println(rooms.sumOf { it.sector })
    println(rooms.find { it.name == "northpole object storage" }!!.sector)
}
