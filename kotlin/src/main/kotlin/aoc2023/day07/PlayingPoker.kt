package aoc2023.day07

import library.Year
import library.readData
import library.sum

enum class HandType {
    HIGH_CARD,
    ONE_PAIR,
    TWO_PAIR,
    THREE_OF_A_KIND,
    FULL_HOUSE,
    FOUR_OF_A_KIND,
    FIVE_OF_A_KIND
}

fun getType(hand: String, jokers: Boolean): HandType {
    val cards = group(hand)
    val keys = cards.keys
    val distinct = keys.size

    if (distinct == 1)
        return HandType.FIVE_OF_A_KIND
    if (distinct == 2) {
        if (jokers && keys.any { it == 'J' })
            return HandType.FIVE_OF_A_KIND
        if (keys.any { cards[it] == 4 })
            return HandType.FOUR_OF_A_KIND
        return HandType.FULL_HOUSE
    }
    if (distinct == 3) {
        if (keys.any { cards[it] == 3 }) {
            if (jokers && keys.any { it == 'J' })
                return HandType.FOUR_OF_A_KIND
            return HandType.THREE_OF_A_KIND
        }
        if (jokers && keys.any { it == 'J' }) {
            if (cards['J']!! == 2)
                return HandType.FOUR_OF_A_KIND
            return HandType.FULL_HOUSE
        }
        return HandType.TWO_PAIR
    }
    if (distinct == 4) {
        if (jokers && keys.any { it == 'J' })
            return HandType.THREE_OF_A_KIND
        return HandType.ONE_PAIR
    }

    if (jokers && keys.any { it == 'J' })
        return HandType.ONE_PAIR
    return HandType.HIGH_CARD
}

val CARDS = listOf('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')

fun getCardValue(card: Char, jokers: Boolean): Int {
    if (jokers && card == 'J') return -1
    return CARDS.indexOf(card)
}

fun group(hand: String): Map<Char, Int> {
    val result = mutableMapOf<Char, Int>()
    for (c in hand) {
        if (c !in result) result[c] = 1
        else result[c] = result[c]!! + 1
    }
    return result
}

data class Hand(
    val hand: String,
    val bid: Int
) {
    val type = getType(hand, false)
    val jokerType = getType(hand, true)
}

fun compare(left: Hand, right: Hand, jokers: Boolean): Int {
    val extractor =
        if (jokers) { hand: Hand -> hand.jokerType }
        else { hand: Hand -> hand.type }
    val compareType = extractor(left).ordinal
        .compareTo(extractor(right).ordinal)
    if (compareType != 0) return compareType

    for (i in 0..4) {
        val compareCard = getCardValue(left.hand[i], jokers)
            .compareTo(getCardValue(right.hand[i], jokers))
        if (compareCard != 0) return compareCard
    }

    return 0
}

fun comparator(jokers: Boolean) = { left: Hand, right: Hand -> compare(left, right, jokers) }

fun calculateWinnings(data: List<Hand>, jokers: Boolean): Long =
    data.sortedWith(comparator(jokers))
        .mapIndexed { index, hand -> (index + 1L) * hand.bid }
        .sum()

fun main() {
    println("Hello, Advent of Code!")

    val hands = readData(Year._2023, 7)
        .map { it.split(" ") }
        .map { Hand(it[0], it[1].toInt()) }

    println(calculateWinnings(hands, false))
    println(calculateWinnings(hands, true))
}
