package aoc2024

import library.Year
import library.geometry.ints.Coordinate
import library.parseRegex
import library.readData

fun move(
    robots: Set<Pair<Coordinate, Coordinate>>,
    seconds: Int = 1,
    room: Coordinate = Coordinate(101, 103)
): List<Coordinate> {
    val result = mutableListOf<Coordinate>()
    for (robot in robots) {
        val x = (((robot.first.x + (robot.second.x * seconds)) % room.x) + room.x) % room.x
        val y = (((robot.first.y + (robot.second.y * seconds)) % room.y) + room.y) % room.y
        result.add(Coordinate(x, y))
    }
    return result
}

fun countQuadrants(robots: List<Coordinate>, room: Coordinate = Coordinate(101, 103)): Long {
    val xHalf = room.x / 2
    val yHalf = room.y / 2

    val topLeft = robots.count { it.x < xHalf && it.y < yHalf }.toLong()
    val topRight = robots.count { it.x > xHalf && it.y < yHalf }.toLong()
    val bottomLeft = robots.count { it.x < xHalf && it.y > yHalf }.toLong()
    val bottomRight = robots.count { it.x > xHalf && it.y > yHalf }.toLong()

    return topLeft * topRight * bottomLeft * bottomRight
}

fun main() {
    val exampleRoom = Coordinate(11, 7)
    val data = readData(Year._2024, 14)
        .parseRegex("p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)") {
            Pair(
                Coordinate(it[0].toInt(), it[1].toInt()),
                Coordinate(it[2].toInt(), it[3].toInt())
            )
        }.toSet()

    println(countQuadrants(move(data, 100)))
}
