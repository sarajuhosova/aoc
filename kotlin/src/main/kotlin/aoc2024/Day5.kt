package aoc2024

import library.Year
import library.readData

fun List<Int>.isOrdered(pages: Map<Int, List<Int>>): Boolean {
    val relevant = pages.filter { it.key in this }
    for (i in this.indices) {
        val elem = this[i]
        if (elem !in relevant) continue
        val sublist = this.subList(0, i)
        if (relevant[elem]!!.any { it in sublist }) return false
    }
    return true
}

fun Int.isBefore(other: Int, pages: Map<Int, List<Int>>): Int {
    if (this in pages && pages[this]!!.contains(other))
        return -1
    if (other in pages && pages[other]!!.contains(this))
        return 1
    return 0
}

fun main() {
    val data = readData(Year._2024, 5)

    val pairs = data.takeWhile { it != "" }
        .map { it.split("|") }
        .map { it.first().toInt() to it.last().toInt() }
    val pages = pairs.groupBy { it.first }
        .mapValues { it.value.map { v -> v.second } }

    val updates = data.drop(pairs.size + 1)
        .map { it.split(",").map { s -> s.toInt() } }
        .partition { it.isOrdered(pages) }

    val valid = updates.first
    println(valid.sumOf { it[(it.size / 2)] })

    val fixed = updates.second.map { it.sortedWith { a, b -> a.isBefore(b, pages) } }
    println(fixed.sumOf { it[(it.size / 2)] })
}
