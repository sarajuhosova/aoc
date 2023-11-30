package aoc2022.day16

import library.Year
import library.parseRegex
import library.readData

const val initial = "AA"

class Valve(
    val flowRate : Int,
    val neighbours: List<String>
)

fun clean(data: String): String =
    data.replace("tunnel leads to valve ", "")
        .replace("tunnels lead to valves ", "")

fun parse(data: List<String>): Map<String, Valve> =
    data.map{ it.split("; ") }
        .map { Pair(
            parseRegex("Valve ([A-Z]+) has flow rate=([0-9]+)", it[0]),
            clean(it[1]).split(", ")
        ) }
        .associate { it.first[0] to Valve(it.first[1].toInt(), it.second) }

fun main() {
    val valves = parse(readData(Year._2022, "day16_e.txt"))

    val largest = findLargestSmarter(valves, 30)
    println(largest)
    println(findLargestSmarterWithElephant(valves, 26, largest))
}
