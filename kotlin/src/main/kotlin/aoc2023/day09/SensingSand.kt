package aoc2023.day09

import library.Year
import library.readData
import library.sum
import library.tail

fun getDiff(values: List<Long>): List<Long> {
    val result = mutableListOf<Long>()
    var previous = values[0]
    for (value in values.tail()) {
        result.add(value - previous)
        previous = value
    }
    return result
}

fun getDiffs(reading: List<Long>): List<List<Long>> {
    val lists = mutableListOf<List<Long>>()
    lists.add(reading)
    while (!lists[0].all { it == 0L }) {
        lists.add(0, getDiff(lists[0]))
    }
    return lists
}

fun getNext(diffs: List<List<Long>>): Long {
    var toAdd = 0L
    for (diff in diffs.tail()) {
        toAdd += diff[diff.size - 1]
    }
    return toAdd
}

fun getFirst(diffs: List<List<Long>>): Long {
    var toSubtract = 0L
    for (diff in diffs.tail()) {
        toSubtract = diff[0] - toSubtract
    }
    return toSubtract
}

fun getNextValue(readings: List<List<Long>>): Long =
    readings.map { getNext(getDiffs(it)) }.sum()

fun getPreviousValue(readings: List<List<Long>>): Long =
    readings.map { getFirst(getDiffs(it)) }.sum()

fun main() {
    val readings = readData(Year._2023, 9)
        .map { it.split(" ").map { i -> i.toLong() } }

    println(getNextValue(readings))
    println(getPreviousValue(readings))
}
