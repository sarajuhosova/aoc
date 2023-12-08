package aoc2023.day08

import library.Year
import library.parseRegex
import library.readData
import java.util.*

enum class Direction(val get: (Pair<String, String>) -> String) {
    LEFT({ p -> p.first }),
    RIGHT({ p -> p.second });
}

fun parse(line: String): List<Direction> =
    line.map { if (it == 'R') Direction.RIGHT else Direction.LEFT }

typealias DesertMap = Map<String, Pair<String, String>>

fun DesertMap.get(key: String, direction: Direction): String =
    direction.get(this[key]!!)

fun parse(lines: List<String>): DesertMap {
    val map = mutableMapOf<String, Pair<String, String>>()
    for (line in lines) {
        val regex = parseRegex("(.{3}) = \\((.{3}), (.{3})\\)", line)
        map[regex[0]] = Pair(regex[1], regex[2])
    }
    return map
}

fun navigateCamel(directions: List<Direction>, map: DesertMap): Int {
    var current = "AAA"
    var count = 0
    while (current != "ZZZ") {
        val next = map.get(current, directions[count % directions.size])
        if (next == current) return -1
        current = next
        count++
    }
    return count
}

typealias NextZ = Pair<String, Long>

fun getNextZ(start: String, directions: List<Direction>, map: DesertMap): NextZ {
    var current = start
    var count = 0L
    do {
        val next = map.get(current, directions[(count % directions.size).toInt()])
        if (next == current) return Pair("", -1L)
        current = next
        count++
    } while (!current.endsWith("Z"))
    return Pair(current, count)
}

fun getFirstState(directions: List<Direction>, map: DesertMap): MutableList<NextZ> {
    val start = mutableListOf<NextZ>()
    for (end in map.keys.filter { it.endsWith("A") }) {
        start.add(getNextZ(end, directions, map))
    }
    return start
}

fun getNextZs(
    directions: List<Direction>, map: DesertMap
): Map<String, List<NextZ>> {
    val next = mutableMapOf<String, List<NextZ>>()
    for (end in map.keys.filter { it.endsWith("Z") }) {
        val list = mutableListOf<NextZ>()
        for (i in directions.indices) {
            list.add(getNextZ(end, directions, map))
            Collections.rotate(directions, 1)
        }
        next[end] = list
    }
    return next
}

fun isFound(state: List<NextZ>): Boolean =
    state.all { it.second == state[0].second }

fun updateState(state: MutableList<NextZ>, nextZs: Map<String, List<NextZ>>) {
    val smallest = state.minBy { it.second }

    val possible = nextZs[smallest.first]!!
    val next = possible[(smallest.second % possible.size).toInt()]

    state[state.indexOf(smallest)] = Pair(next.first, smallest.second + next.second)
}

fun navigateGhost(directions: List<Direction>, map: DesertMap): Long {
    val nextZs = getNextZs(directions, map)

    val state = getFirstState(directions, map)

    while (!isFound(state)) {
        updateState(state, nextZs)
    }

    return state[0].second
}

fun main() {
    val data = readData(Year._2023, 8)

    val directions = parse(data[0])
    val map = parse(data.drop(2))

//    println(navigateCamel(directions, map))
    println(navigateGhost(directions, map))
}
