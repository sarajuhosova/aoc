package aoc2024

import library.Year
import library.readData
import kotlin.math.abs

fun List<Int>.getDifferences(): List<Int> {
    val result = mutableListOf<Int>()
    for (i in 1..this.lastIndex) {
        result.add(this[i] - this[i - 1])
    }
    return result
}

fun List<Int>.areDifferencesSafe(): Boolean {
    if (this.isEmpty()) return true
    val positive = this[0] >= 0
    return this.all { diff -> (if (positive) diff > 0 else diff < 0) && 1 <= abs(diff) && abs(diff) <= 3 }
}

fun List<Int>.isReportSafe(): Boolean =
    this.getDifferences().areDifferencesSafe()

fun List<Int>.isDampenedReportSafe(): Boolean {
    for (i in this.indices) {
        val sublist = this.subList(0, i) + this.subList(i + 1, this.size)
        if (sublist.isReportSafe()) return true
    }
    return false
}

fun findSafeReports(reports: List<List<Int>>): Int =
    reports.count { it.isReportSafe() }


fun findDampenedSafeReports(reports: List<List<Int>>): Int =
    reports.count { it.isDampenedReportSafe() }

fun main() {
    val data = readData(Year._2024, 2)
        .map { it.split(" ").map { i -> i.toInt() } }

    println(findSafeReports(data))
    println(findDampenedSafeReports(data))
}
