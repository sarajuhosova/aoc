package library.geometry.ints

import java.util.*
import kotlin.math.sign

fun Map<Coordinate, Char>.draw(default: Char = ' '): String {
    val smallestX = this.keys.minOf { it.x }
    val largestX = this.keys.maxOf { it.x }
    val smallestY = this.keys.minOf { it.y }
    val largestY = this.keys.maxOf { it.y }

    val result = mutableListOf<String>()
    for (x in smallestX .. largestX) {
        var string = ""
        for (y in smallestY .. largestY) {
            string += this.getOrDefault(Coordinate(x, y), default)
        }
        result.add(string)
    }
    return result.joinToString("\n")
}

fun Set<Coordinate>.shortest(start: Coordinate): Map<Coordinate, List<Coordinate>> {
    val distances = mutableMapOf<Coordinate, List<Coordinate>>()
    distances[start] = mutableListOf(start)

    val queue = PriorityQueue<Pair<Coordinate, Int>>(compareBy { it.second })
    queue.add(Pair(start, 0))

    while (queue.isNotEmpty()) {
        val (position, distance) = queue.poll()
        val neighbours = position.getNeighbours()
            .filter { it in this }

        val path = distances[position]!!
        for (neighbour in neighbours) {
            if (distance + 1 < (distances[neighbour]?.size ?: Int.MAX_VALUE)) {
                distances[neighbour] = (path + listOf(neighbour))
                queue.add(neighbour to (distance + 1))
            }
        }
    }

    return distances
}
