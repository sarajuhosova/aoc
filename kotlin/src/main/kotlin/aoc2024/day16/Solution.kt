package aoc2024.day16

import library.Year
import library.geometry.Direction
import library.geometry.ints.Coordinate
import library.readData
import java.util.*

val START_DIRECTION = Direction.RIGHT.counterClockwise()

fun parseMap(input: List<String>): Pair<Set<Coordinate>, Pair<Coordinate, Coordinate>> {
    var start: Coordinate? = null
    var end: Coordinate? = null
    val path = mutableSetOf<Coordinate>()

    for (row in 1 until input.lastIndex) {
        for (col in 1 until input[row].lastIndex) {
            val coordinate = Coordinate(row - 1, col - 1)
            when (input[row][col]) {
                '.' -> path.add(coordinate)
                'S' -> {
                    start = coordinate
                    path.add(coordinate)
                }
                'E' -> {
                    end = coordinate
                    path.add(coordinate)
                }
            }
        }
    }

    return Pair(path, Pair(start!!, end!!))
}

fun Set<Coordinate>.shortest(start: Coordinate, end: Coordinate): Long {
    val map = this.associateWith { mutableMapOf<Direction, Long>() }.toMutableMap()
    map[start]!![START_DIRECTION] = 0L

    val queue = PriorityQueue<Pair<Pair<Coordinate, Direction>, Long>>(compareBy { it.second })
    queue.add((start to START_DIRECTION) to 0L)

    while (queue.isNotEmpty()) {
        val (heading, distance) = queue.poll()
        val neighbours = heading.first.getNeighbourMap().filterValues { it in this }
        for ((direction, neighbour) in neighbours) {
            val nDistance = distance + (if (direction == heading.second) 1L else 1001L)
            val distances = map[neighbour]!!
            if (direction !in distances || distances[direction]!! > nDistance) {
                distances[direction] = nDistance
                queue.add(Pair(Pair(neighbour, direction), nDistance))
            }
        }
    }

    return map[end]!!.map { it.value }.min()
}

fun main() {
    val (path, positions) = parseMap(readData(Year._2024, 16))

    println(path.shortest(positions.first, positions.second))
}
