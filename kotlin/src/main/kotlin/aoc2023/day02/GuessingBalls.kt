package aoc2023.day02

import library.Year
import library.product
import library.readData

val VALID = mapOf(
    Pair("red", 12),
    Pair("green", 13),
    Pair("blue", 14)
)

data class Game(
    val input: List<String>
) {
    val id = input[0].dropWhile { it !in '0'..'9' }.toInt()

    private val turns = input[1].split("; ").map { parseTurn(it) }

    private fun parseTurn(input: String): Map<String, Int> = input
        .split(", ")
        .map { it.split(" ") }
        .associate { it[1] to it[0].toInt() }

    fun isValid(): Boolean = turns
        .all { t -> VALID.keys .all { k ->
            (k !in t) || (t[k]!! <= VALID[k]!!)
        } }

    private fun findMinimumOf(colour: String): Int = turns
        .maxOf { if (colour in it) it[colour]!! else 0 }

    fun getMinimalPower(): Int = VALID.keys.map { findMinimumOf(it) }.product()
}

fun findValid(data: List<Game>): Int = data
    .filter { it.isValid() }
    .sumOf { it.id }

fun findMinimalPowers(data: List<Game>): Int = data.sumOf { it.getMinimalPower() }

fun main() {
    val data = readData(Year._2023, 2)
        .map { Game(it.split(": ")) }

    println(findValid(data))
    println(findMinimalPowers(data))
}
