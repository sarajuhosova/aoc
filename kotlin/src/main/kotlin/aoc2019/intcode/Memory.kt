package aoc2019.intcode

typealias Memory = Array<Int>

fun Memory.copy() = this.map { it }.toTypedArray()
