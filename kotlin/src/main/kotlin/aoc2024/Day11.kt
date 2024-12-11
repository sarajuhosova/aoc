package aoc2024

import library.Year
import library.readFirst

fun Long.split(): Pair<Long, Long> {
    val string = this.toString()
    return Pair(
        string.substring(0, string.length / 2).toLong(),
        string.substring(string.length / 2).toLong()
    )
}

fun MutableMap<Long, Long>.add(stone: Long, amount: Long) {
    this[stone] = (this[stone] ?: 0L) + amount
}

fun blink(stones: Map<Long, Long>): Map<Long, Long> {
    val result = mutableMapOf<Long, Long>()
    for ((engraving, amount) in stones) {
        if (engraving == 0L) result.add(1, amount)
        else {
            if (engraving.toString().length % 2 == 0) {
                val (left, right) = engraving.split()
                result.add(left, amount)
                result.add(right, amount)
            } else result.add(engraving * 2024, amount)
        }
    }
    return result
}

fun count(stones: Map<Long, Long>, repeat: Int): Long {
    var mutable = stones
    repeat (repeat) {
        mutable = blink(mutable)
    }
    return mutable.map { (_, v) -> v }.sum()
}

fun main() {
    val stones = readFirst(Year._2024, 11)
        .split(" ").map { it.toLong() }
        .associateWith { 1L }

    println(count(stones, 25))
    println(count(stones, 75))
}
