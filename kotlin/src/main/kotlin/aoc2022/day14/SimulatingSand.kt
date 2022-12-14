package aoc2022.day14

import library.Year
import library.head
import library.readData
import library.tail

data class Point(
    val x: Int = 500,
    val y: Int = 0
)

fun parse(data: List<String>): List<List<Point>> =
    data.map { it.split(" -> ").map { point ->
        point.split(",").map { s -> s.toInt() }
    }.map { point -> Point(point[0], point[1]) } }

fun getYRange(last: Point, next: Point): IntProgression =
    if (last.y < next.y) (last.y + 1..next.y)
    else (last.y - 1 downTo next.y)

fun getXRange(last: Point, next: Point): IntProgression =
    if (last.x < next.x) (last.x + 1..next.x)
    else (last.x - 1 downTo next.x)

fun drawLines(points: List<Point>): List<Point> =
    points.tail().fold(mutableListOf(points.head())) { acc, point ->
        val last = acc.last()
        if (last.x == point.x) getYRange(last, point).forEach { y -> acc.add(Point(point.x, y)) }
        if (last.y == point.y) getXRange(last, point).forEach { x -> acc.add(Point(x, point.y)) }
        acc
    }

fun fillSpace(data: List<List<Point>>): Set<Point> =
    data.flatMap { drawLines(it) }.toSet()

fun nextPosition(position: Point, paths: Set<Point>, grain: Set<Point>): Point? {
    val down = Point(position.x, position.y + 1)
    if (!paths.contains(down) && !grain.contains(down)) return down

    val left = Point(position.x - 1, position.y + 1)
    if (!paths.contains(left) && !grain.contains(left)) return left

    val right = Point(position.x + 1, position.y + 1)
    if (!paths.contains(right) && !grain.contains(right)) return right

    return null
}

fun simulateSandFall(paths: Set<Point>, grain: Set<Point>, boundary: Int): Point? {
    var position = Point()
    while (true) {
        if (position.y + 1 > boundary) return null

        val next = nextPosition(position, paths, grain) ?: break

        position = next
    }

    return position
}

fun simulateSandHeap(paths: Set<Point>, grain: Set<Point>, boundary: Int): Point {
    var position = Point()
    while (true) {
        if (position.y + 1 >= boundary + 2) break

        val next = nextPosition(position, paths, grain) ?: break

        position = next
    }

    return position
}

fun simulateSand(
    paths: Set<Point>,
    simulator: (Set<Point>, Set<Point>, Int) -> Point?
): Set<Point> {
    val boundary = paths.maxOfOrNull { it.y }!!

    val grain = mutableSetOf<Point>()
    while (true) {
        if (grain.contains(Point())) return grain

        val next = simulator(paths, grain, boundary) ?: return grain

        grain.add(next)
    }
}

fun main() {
    val paths = fillSpace(parse(readData(Year._2022, "day14.txt")))

    println(simulateSand(paths) { p, g, b -> simulateSandFall(p, g, b) }.size)
    println(simulateSand(paths) { p, g, b -> simulateSandHeap(p, g, b) }.size)
}
