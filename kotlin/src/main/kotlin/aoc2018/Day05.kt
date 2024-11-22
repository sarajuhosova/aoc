package aoc2018

import library.Year
import library.readFirst
import kotlin.math.abs

val DIFFERENCE = abs('A' - 'a')

fun reducePolymerNaive(polymer: String): Int {
    var state = polymer
    while (true) {
        var newState = ""
        var i = 0
        while (i < state.length) {
            if (i != state.lastIndex && abs(state[i] - state[i + 1]) == DIFFERENCE) {
                i += 2
            } else {
                newState += state[i]
                i++
            }
        }
        if (newState.length == state.length) return newState.length
        state = newState
    }
}

fun findFirstReactionIndex(polymer: String): Int? {
    var i = 0
    while (i < polymer.length - 1) {
        if (abs(polymer[i] - polymer[i + 1]) == DIFFERENCE) {
            return i
        }
        i++
    }
    return null
}

fun reducePolymer(input: String): Int {
    var polymer = input

    while (true) {
        val nextIndex = findFirstReactionIndex(polymer)
        if (nextIndex == null) return polymer.length

        polymer = polymer.removeRange(nextIndex, nextIndex + 2)
    }
}

fun reduceFixedPolymer(input: String): Int =
    ('a'.. 'z')
        .map { unit -> reducePolymer(
            input.filter { c -> c != unit && abs(c - unit) != DIFFERENCE }
        ) }
        .min()

fun main() {
    val polymer = readFirst(Year._2018, 5)

    println(reducePolymer(polymer))
    println(reduceFixedPolymer(polymer))
}
