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

fun findAntinodes(coordinates: List<Coordinate>): Set<Coordinate> {
    val result = mutableSetOf<Coordinate>()

    for (x in coordinates.indices) {
        val left = coordinates[x]
        for (y in (x + 1) until coordinates.size) {
            val right = coordinates[y]
            val deltaX = right.x - left.x
            val deltaY = right.y - left.y

            result.add(Coordinate(left.x - deltaX, left.y - deltaY))
            result.add(Coordinate(right.x + deltaX, right.y + deltaY))
        }
    }

    return result
}

fun main() {
    val data = readData(Year._2024, 8)
    val antennas = parseAntennas(data)

    val antinodes = antennas.map { findAntinodes(it.value) }
        .reduce { a, b -> a union b }
        .filter { it.inBounds(Coordinate(data[0].length, data.size)) }

    println(antinodes.associateBy({ it }, { '#' }).draw('.'))

    println(antinodes.size)
}
