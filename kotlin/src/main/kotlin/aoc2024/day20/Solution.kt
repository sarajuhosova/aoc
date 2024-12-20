package aoc2024.day20

import aoc2024.day16.parseMap
import library.Year
import library.geometry.Direction
import library.geometry.ints.Coordinate
import library.geometry.ints.shortest
import library.readData
import java.util.*

fun List<Coordinate>.findCheats(cutoff: Int = 100): Set<Pair<Coordinate, Direction>> {
    val result = mutableSetOf<Pair<Coordinate, Direction>>()

    for (i in this.indices) {
        val position = this[i]
        position.getNeighbourMap(2)
            .filter { (_, n) -> n in this && i + cutoff < this.indexOf(n) }
            .forEach { (d, _) -> result.add(position to d) }
    }

    return result
}

fun main() {
    val (path, positions) = parseMap(readData(Year._2024, 20))

    val shortest = path.shortest(positions.first)[positions.second]!!

    println(shortest.findCheats().size)
}
