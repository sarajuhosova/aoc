package aoc2024.day14

import library.Year
import library.geometry.ints.Coordinate
import library.geometry.ints.draw
import library.parseRegex
import library.readData

val ROOM = Coordinate(101, 103)

fun List<Int>.inARow(min: Int): Boolean {
    var count = 1
    for (i in 1 until size) {
        if (this[i] - this[i - 1] == 1) {
            count++
            if (count >= min) return true
        }
        else count = 1
    }
    return false
}

fun List<Coordinate>.inARow(): Boolean =
    this.groupBy { it.y }.map { it.value.map { c -> c.x } }.any { it.inARow(3) }

fun List<Coordinate>.inAColumn(): Boolean =
    this.groupBy { it.x }.map { it.value.map { c -> c.y } }.any { it.inARow(3) }

fun move(
    robots: List<Pair<Coordinate, Coordinate>>,
    seconds: Int = 1,
    room: Coordinate = Coordinate(101, 103)
): List<Pair<Coordinate, Coordinate>> {
    val result = mutableListOf<Pair<Coordinate, Coordinate>>()
    for (robot in robots) {
        val x = (((robot.first.x + (robot.second.x * seconds)) % room.x) + room.x) % room.x
        val y = (((robot.first.y + (robot.second.y * seconds)) % room.y) + room.y) % room.y
        result.add(Pair(Coordinate(x, y), robot.second))
    }
    return result
}

fun countQuadrants(robots: List<Coordinate>, room: Coordinate = ROOM): Long {
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
        }

    println(countQuadrants(move(data, 100).map { it.first }))

    val map = mutableMapOf(Pair(Coordinate.origin(), '.'), Pair(ROOM, '.'))
    map.putAll(move(data, 7520).map { it.first }.associateWith { '#' })
    println(map.draw('.'))

    // I CAN'T EVEN EXPLAIN WHAT HAPPENED HERE

    val file = java.io.File("src/main/resources/aoc2024/day14/out.txt")
    file.writeText("hello!")

    var i = 4995 % 101
    var printed = 0
    var robots = move(data, i)
    val origin = Coordinate.origin()
    while (printed < 100) {
        i += 101
        robots = move(robots, 101)

        val positions = robots.map { it.first }
        if (positions.inARow() && positions.inAColumn()) {
            printed++
            val map = mutableMapOf(Pair(origin, '.'), Pair(ROOM, '.'))
            map.putAll(positions.associateWith { '#' })
            file.appendText("$i\n" + map.draw('.') + "\n\n")
        }
    }
}
