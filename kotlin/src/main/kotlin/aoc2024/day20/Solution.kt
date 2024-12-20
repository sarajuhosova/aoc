package aoc2024.day20

import aoc2024.day16.parseMap
import library.Year
import library.geometry.ints.Coordinate
import library.geometry.ints.shortest
import library.readData

fun List<Coordinate>.findCheats(max: Int = 20, cutoff: Int = 100): Int {
    var result = 0

    for (i in this.indices) {
        for (j in (i + cutoff)..this.lastIndex) {
            val distance = this[j].manhattanDistance(this[i])
            if (distance <= max && j >= i + distance + cutoff) result++
        }
    }

    return result
}

fun main() {
    val (path, positions) = parseMap(readData(Year._2024, 20))

    val shortest = path.shortest(positions.first)[positions.second]!!

    println(shortest.findCheats(2))
    println(shortest.findCheats(20))
}
