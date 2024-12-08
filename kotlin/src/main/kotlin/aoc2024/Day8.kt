package aoc2024

import library.Coordinate
import library.Year
import library.draw
import library.readData

fun parseAntennas(data: List<String>): Map<Char, List<Coordinate>> {
    val map = mutableMapOf<Char, MutableSet<Coordinate>>()
    for (row in data.indices) {
        for (col in data[0].indices) {
            val c = data[row][col]
            if (c == '.') continue
            if (!map.containsKey(c)) map[c] = mutableSetOf()
            map[c]!!.add(Coordinate(col, row))
        }
    }
    return map.mapValues { it.value.toList().sortedBy { c -> c.x } }
}

fun findAntinodes(
    coordinates: List<Coordinate>, bounds: Coordinate, repeat: Boolean = false
): Set<Coordinate> {
    val result = mutableSetOf<Coordinate>()

    for (x in coordinates.indices) {
        val left = coordinates[x]
        for (y in (x + 1) until coordinates.size) {
            val right = coordinates[y]
            val delta = Coordinate(right.x - left.x, right.y - left.y)

            if (repeat) {
                result.add(left)
                result.add(right)
            }

            var smaller = left - delta
            while (smaller.inBounds(bounds)) {
                result.add(smaller)
                if (!repeat) break
                smaller -= delta
            }
            var larger = right + delta
            while (larger.inBounds(bounds)) {
                result.add(larger)
                if (!repeat) break
                larger += delta
            }
        }
    }

    return result
}

fun main() {
    val data = readData(Year._2024, 8)
    val antennas = parseAntennas(data)
    val bounds = Coordinate(data[0].length, data.size)

    val singleAntinodes = antennas.map { findAntinodes(it.value, bounds) }
        .reduce { a, b -> a union b }
    println(singleAntinodes.associateBy({ it }, { '#' }).draw('.'))
    println(singleAntinodes.size)

    val allAntinodes = antennas.map { findAntinodes(it.value, bounds, true) }.reduce { a, b -> a union b }
    println(allAntinodes.associateBy({ it }, { '#' }).draw('.'))
    println(allAntinodes.size)
}
