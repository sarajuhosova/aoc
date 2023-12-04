package aoc2023.day04

import library.Year
import library.readData

data class ScratchCard(
    val winning: Set<Int>,
    val yours: Set<Int>
) {
    val matching: Int = yours.intersect(winning).size
}

fun parse(input: String): ScratchCard {
    val lists = input.split(": ")[1]
        .split(" | ").map { split ->
            split.split(" ")
                .filter { it != "" }
                .map { it.toInt() }
        }
    return ScratchCard(lists[0].toSet(), lists[1].toSet())
}

fun points(exponent: Int): Long {
    if (exponent < 1) return 0L
    if (exponent == 1) return 1L
    return points(exponent - 1) * 2
}

fun exponentialValue(cards: List<ScratchCard>): Long =
    cards.sumOf { points(it.matching) }

fun cards(index: Int, cards: List<ScratchCard>): Long {
    val next = cards[index].matching
    return next + ((index + 1)..(index + next)).sumOf { cards(it, cards) }
}

fun duplicatedCards(cards: List<ScratchCard>): Long =
    cards.indices.sumOf { cards(it, cards) } + cards.size

fun main() {
    println("Hello, Advent of Code!")

    val data = readData(Year._2023, 4).map { parse(it) }

    println(exponentialValue(data))
    println(duplicatedCards(data))
}
