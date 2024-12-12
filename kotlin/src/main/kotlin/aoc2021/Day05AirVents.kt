package aoc2021

import library.*
import library.geometry.Coordinate
import library.geometry.Line

fun countOverlapsNaive(lines: List<Line>): Int {
    val positions = mutableSetOf<Coordinate>()
    val overlaps = mutableSetOf<Coordinate>()

    for (line in lines) {
        val generated = line.generatePositions()
        overlaps.addAll(positions intersect generated)
        positions.addAll(generated)
    }

    return overlaps.size
}

fun main() {
    val data = readData(Year._2021, 5).parseRegex("([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)") {
        Line(Coordinate(it[0].toInt(), it[1].toInt()), Coordinate(it[2].toInt(), it[3].toInt()))
    }

    println(countOverlapsNaive(data.filter { it.straight }))
    println(countOverlapsNaive(data))
}
