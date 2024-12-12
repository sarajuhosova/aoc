package aoc2024

import library.geometry.Coordinate
import library.Year
import library.readData

fun List<String>.toIntArray(): Array<Array<Int>> =
    Array(this.size) { row -> Array(this[row].length) { col -> this[row][col].digitToInt() } }

fun Array<Array<Int>>.getTrailheads(): Set<Coordinate> =
    this.flatMapIndexed { row, line -> line.mapIndexed { col, i -> Coordinate(col, row) to i } }
        .filter { it.second == 0 }.map { it.first }.toSet()

fun Array<Array<Int>>.at(coordinate: Coordinate): Int = this[coordinate.y][coordinate.x]

fun Array<Array<Int>>.findTrails(head: Coordinate): Set<List<Coordinate>> {
    if (this.isEmpty()) return emptySet()
    val bounds = Coordinate(this[0].size, this.size)

    var trails = listOf(listOf(head))

    for (i in 1..9) {
        trails = trails.flatMap { trail ->
            trail.last().getNeighbours()
                .filter { it.inBounds(bounds) && this.at(it) == i }
                .map { trail + listOf(it) }
        }
    }

    return trails.toSet()
}

fun main() {
    val map = readData(Year._2024, 10).toIntArray()

    val trails = map.getTrailheads().map { map.findTrails(it) }
    println(trails.sumOf { it.map { trail -> trail.last() }.toSet().size })
    println(trails.sumOf { it.size })
}
