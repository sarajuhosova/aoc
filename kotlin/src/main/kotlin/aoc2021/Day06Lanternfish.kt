package aoc2021

import library.Year
import library.readFirst

fun simulateLanternfish(fish: List<Int>, days: Int): Long {
    val counters = Array(9) { 0L }.toMutableList()
    for (f in fish) counters[f] = counters[f] + 1 // counters[f] += 1 throws a compiler error?

    repeat (days) {
        val zeros = counters.removeFirst()
        counters[6] = counters[6] + zeros
        counters.add(zeros)
    }

    return counters.sum()
}

fun main() {
    val fish = readFirst(Year._2021, 6)
        .split(",").map { it.toInt() }

    println(simulateLanternfish(fish, 80))
    println(simulateLanternfish(fish, 256))
}
