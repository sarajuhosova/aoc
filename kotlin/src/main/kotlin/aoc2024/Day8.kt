package aoc2024

import library.geometry.Coordinate
import library.Year
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

            if (!repeat) {
                val smaller = left - delta
                if (smaller.inBounds(bounds)) result.add(smaller)
                val larger = right + delta
                if (larger.inBounds(bounds)) result.add(larger)
                continue
            }

            var smaller = left
            do {
                result.add(smaller)
                smaller -= delta
            } while (smaller.inBounds(bounds))
            var larger = right
            do {
                result.add(larger)
                larger += delta
            } while (larger.inBounds(bounds))
        }
    }

    return result
}

fun main() {
    val data = readData(Year._2024, 8)
    val antennas = parseAntennas(data)
    val bounds = Coordinate(data[0].length, data.size)

    val antinodes = antennas.map {
        findAntinodes(it.value, bounds) to findAntinodes(it.value, bounds, true)
    }.reduce { a, b -> (a.first union b.first) to (a.second union b.second) }

    println(antinodes.first.size)
    println(antinodes.second.size)
}
