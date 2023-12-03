package aoc2023.day03

import library.Year
import library.readData
import library.sum

typealias Position = Pair<Int, Int>

typealias EngineParts = Map<Position, String>
typealias Symbols = Map<Position, Pair<Char, Int>>

fun parse(input: List<String>): Pair<EngineParts, Symbols> {
    val parts = mutableMapOf<Position, String>()
    val symbols = mutableMapOf<Position, Pair<Char, Int>>()

    var id = 0
    for (i in input.indices) {
        var j = 0
        while (j < input[i].length) {
            val c = input[i][j]
            if (c in '0'..'9') {
                var value = "" + c
                val pos = Pair(i, j)
                while (j + 1 < input[i].length && input[i][j + 1] in '0'..'9') {
                    value += input[i][j + 1]
                    j++
                }
                parts[pos] = value
            } else if (c != '.') {
                symbols[Pair(i, j)] = Pair(c, id)
                id++
            }
            j++
        }
    }

    return Pair(parts, symbols)
}

fun isAdjacentTo(pos: Position, part: String, symbols: Symbols): Pair<Char, Int>? {
    val start = pos.second - 1
    val end = pos.second + part.length
    // check the row above
    for (j in start..end) {
        val p = Pair(pos.first - 1, j)
        if (p in symbols) return symbols[p]
    }
    // check the left
    if (Pair(pos.first, start) in symbols) return symbols[Pair(pos.first, start)]
    // check the right
    if (Pair(pos.first, end) in symbols) return symbols[Pair(pos.first, end)]
    // check the row below
    for (j in start..end) {
        val p = Pair(pos.first + 1, j)
        if (p in symbols) return symbols[p]
    }
    return null
}

fun findParts(parts: EngineParts, symbols: Symbols): Long {
    return parts.keys
        .filter { isAdjacentTo(it, parts[it]!!, symbols) != null }
        .sumOf { parts[it]!!.toLong() }
}

fun findGears(parts: EngineParts, symbols: Symbols): Long {
    val gears = mutableMapOf<Int, Pair<Long, Int>>()
    for ((pos, part) in parts) {
        val adjacent = isAdjacentTo(pos, part, symbols)
        if (adjacent != null && adjacent.first == '*') {
            if (adjacent.second !in gears) {
                gears[adjacent.second] = Pair(part.toLong(), 1)
            } else {
                val (ratio, nParts) = gears[adjacent.second]!!
                if (nParts < 2) {
                    gears[adjacent.second] = Pair(ratio * part.toLong(), nParts + 1)
                }
            }
        }
    }
    return gears.keys
        .map { gears[it]!! }
        .filter { it.second == 2 }
        .map { it.first }
        .sum()
}

fun main() {
    println("Hello, Advent of Code!")

    val (parts, symbols) = parse(readData(Year._2023, 3))

    println(findParts(parts, symbols))
    println(findGears(parts, symbols))
}
