package aoc2024

import library.*
import library.geometry.*
import library.geometry.ints.Coordinate
import library.geometry.ints.area
import library.geometry.ints.countSides
import library.geometry.ints.perimeter

fun Array<CharArray>.get(coordinate: Coordinate): Char =
    this[coordinate.y][coordinate.x]

fun getRegions(map: Array<CharArray>): Set<Set<Coordinate>> {
    val regions = mutableMapOf<Coordinate, MutableSet<Coordinate>>()
    val bounds = Coordinate(map[0].size, map.size)

    for (row in map.indices) {
        for (col in map[0].indices) {
            val coordinate = Coordinate(col, row)
            val char = map.get(coordinate)

            if (coordinate !in regions) regions[coordinate] = mutableSetOf(coordinate)
            val set = regions[coordinate]!!

            val right = coordinate.move(Direction.RIGHT)
            if (right.inBounds(bounds) && map.get(right) == char) {
                val rightSet = regions[right] ?: mutableSetOf(right)

                set.addAll(rightSet)
                for (other in rightSet) regions[other] = set
            }

            val up = coordinate.move(Direction.UP)
            if (up.inBounds(bounds) && map.get(up) == char) {
                set.add(up)
                regions[up] = set
            }
        }
    }

    return regions.values.toSet()
}

fun main() {
    val map = readData(Year._2024, 12)
        .map { it.toCharArray() }.toTypedArray()

    val regions = getRegions(map)
    val results = regions.map { Triple(it.area(), it.perimeter(), it.countSides()) }

    println(results.sumOf { it.first * it.second })
    println(results.sumOf { it.first * it.third })
}
