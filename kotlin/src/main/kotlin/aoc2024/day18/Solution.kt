package aoc2024.day18

import library.Year
import library.geometry.ints.Coordinate
import library.readData
import java.util.*

val EXIT = Coordinate(70, 70)
const val kb = 1024

fun shortest(corrupted: Set<Coordinate>, exit: Coordinate = EXIT): Int? {
    val start = Coordinate.origin()
    val bounds = exit + 1

    val distances = mutableMapOf<Coordinate, Int>().withDefault { Int.MAX_VALUE }
    distances[start] = 0

    val queue = PriorityQueue<Pair<Coordinate, Int>>(compareBy { it.second })
    queue.add(Pair(start, 0))

    while (queue.isNotEmpty()) {
        val (position, d) = queue.poll()
        val neighbours = position.getNeighbours()
            .filter { it.inBounds(bounds) && it !in corrupted }

        val distance = d + 1
        for (neighbour in neighbours) {
            if (distance < distances.getValue(neighbour)) {
                distances[neighbour] = distance
                queue.add(neighbour to distance)
            }
        }
    }

    return distances[exit]
}

fun main() {
    val bytes = readData(Year._2024, 18)
        .map { it.split(",") }
        .map { Coordinate(it[0].toInt(), it[1].toInt()) }

    val kilobyte = bytes.take(kb).toSet()
    println(shortest(bytes.take(kb).toSet()))

    var i = kb
    val corrupted = kilobyte.toMutableSet()
    while (i < bytes.size) {
        corrupted.add(bytes[i])
        val shortest = shortest(corrupted)
        if (shortest == null) {
            println(bytes[i])
            break
        }
        i++
    }
}
