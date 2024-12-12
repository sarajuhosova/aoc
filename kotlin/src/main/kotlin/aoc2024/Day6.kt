package aoc2024

import library.geometry.Coordinate
import library.geometry.Direction
import library.Year
import library.readData

fun parse(data: List<String>): Triple<Set<Coordinate>, Coordinate, Coordinate> {
    val obstacles = mutableSetOf<Coordinate>()
    var start: Coordinate? = null
    for (row in data.indices) {
        for (column in data[row].indices) {
            when (data[row][column]) {
                '^' -> start = Coordinate(column, row)
                '#' -> obstacles.add(Coordinate(column, row))
            }
        }
    }
    return Triple(obstacles, start!!, Coordinate(data[0].length, data.size))
}

fun Coordinate.inBounds(x: Int, y: Int): Boolean {
    return this.x in 0..<x && this.y in 0..<y
}

fun walk(
    start: Coordinate,
    obstacles: Set<Coordinate>,
    size: Coordinate
): List<Coordinate> {
    val path = mutableListOf(start)

    var position = start
    var direction = Direction.DOWN
    while (true) {
        var next = position.move(direction)
        while (next in obstacles) {
            direction = direction.counterClockwise()
            next = position.move(direction)
        }

        if (!next.inBounds(size.x, size.y)) break

        position = next
        path.add(position)
    }

    return path
}

fun loops(
    start: Coordinate,
    obstacles: Set<Coordinate>,
    size: Coordinate
): Boolean {
    var direction = Direction.DOWN
    val path = mutableListOf(Pair(start, direction))

    var position = start
    while (true) {
        var next = position.move(direction)
        while (next in obstacles) {
            direction = direction.counterClockwise()
            next = position.move(direction)
        }

        val pair = Pair(next, direction)
        if (pair in path) return true

        if (!next.inBounds(size.x, size.y)) break

        position = next
        path.add(pair)
    }

    return false
}

fun main() {
    val data = readData(Year._2024, 6)
    val (obstacles, start, size) = parse(data)

    val visited = walk(start, obstacles, size).distinct()
    println(visited.size)

    val options = visited.drop(1)
        .filter { loops(start, obstacles + setOf(it), size) }
    println(options.size)
}