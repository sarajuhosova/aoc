package aoc2022.day15

import library.Year
import library.parseRegex
import library.readData
import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

const val TEST = false

fun getValues(): Triple<String, Long, Long> =
    if (TEST) Triple("day15_e.txt", 10L, 20L)
    else Triple("day15.txt", 2000000L, 4000000L)

data class Point(
    val x: Long,
    val y: Long
)

data class Sensor(
    val loc: Point,
    val distance: Long
)

fun parse(data: List<String>): Pair<List<Sensor>, Set<Point>> {
    val parsed =  data.parseRegex(
        "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)",
    ).map { it.map { s -> s.toLong() } }

    return Pair(
        parsed.map { Sensor(Point(it[0], it[1]), abs(it[2] - it[0]) + abs(it[3] - it[1])) },
        parsed.map { Point(it[2], it[3]) }.toSet()
    )
}

fun coversOnLine(line: Long, sensors: List<Sensor>): Set<Point> =
    sensors.filter { sensor ->
        abs(sensor.loc.y - line) <= sensor.distance
    }.fold(mutableSetOf()) { acc, sensor ->
        val left = abs(abs(sensor.loc.y - line) - sensor.distance)
        for (x in ((sensor.loc.x) downTo sensor.loc.x - left)) {
            acc.add(Point(x, line))
        }
        for (x in ((sensor.loc.x + 1).. sensor.loc.x + left)) {
            acc.add(Point(x, line))
        }

        acc
    }

fun unifyRanges(left: Pair<Long, Long>, right: Pair<Long, Long>): Pair<Long, Long>? {
    if (left.first >= right.first) {
        if (left.second <= right.second) return right
        if (left.first <= right.second) return Pair(right.first, left.second)
        return null
    }
    if (right.second <= left.second) return left
    if (right.first <= left.second) return Pair(left.first, right.second)
    return null
}

fun covers(ranges: List<Pair<Long, Long>>): Boolean =
    ranges.size == 1 && ranges[0].first <= 0 && ranges[0].second >= getValues().third

fun findMissing(ranges: List<Pair<Long, Long>>): Long {
    if (ranges.size == 2) {
        return min(ranges[0].second, ranges[1].second) + 1
    }
    if (ranges[0].first > 0) return 0
    return getValues().third
}

fun rangeCoveredOnLine(line: Long, sensors: List<Sensor>): Long? {
    val reaching = sensors.filter { sensor ->
        abs(sensor.loc.y - line) <= sensor.distance
    }

    var ranges = emptyList<Pair<Long, Long>>()
    for (sensor in reaching) {
        val left = abs(abs(sensor.loc.y - line) - sensor.distance)
        var rng = Pair(
            max(0, sensor.loc.x - left),
            min(getValues().third, sensor.loc.x + left)
        )

        val next = mutableListOf<Pair<Long, Long>>()
        for (range in ranges) {
            val new = unifyRanges(rng, range)

            if (new == null) next.add(range)
            else rng = new
        }
        next.add(rng)

        if (covers(next)) return null
        ranges = next
    }

    return if (covers(ranges)) null else findMissing(ranges)
}

fun findBeacon(sensors: List<Sensor>): Point {
    for (line in (0..getValues().third)) {
        val missing = rangeCoveredOnLine(line, sensors)
        if (missing != null) return Point(missing, line)
    }
    return Point(-1, -1)
}

fun main() {
    val data = parse(readData(Year._2022, getValues().first))

    val covers = coversOnLine(getValues().second, data.first).minus(data.second).sortedBy { it.x }
    println(covers.fold(0L) { acc, _ -> acc + 1L })

    val missing = findBeacon(data.first)
    println(missing.x * 4000000 + missing.y)
}
