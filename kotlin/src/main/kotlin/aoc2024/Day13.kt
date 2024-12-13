package aoc2024

import library.Year
import library.geometry.longs.Coordinate
import library.parseByGroup
import library.parseRegex
import library.readData

class Game(val a: Long, val b: Long) {
    val price = a * 3 + b
}

class Machine(val A: Coordinate, val B: Coordinate, val prize: Coordinate) {

    fun cheapest(): Game? {
        // A * a + B * b = prize

        // Ax * a + Bx * b = Px
        // Ay * a + By * b = Py

        // Ay * Ax * a + Ay * Bx * b = Ay * Px
        // Ax * Ay * a + Ax * By * b = Ax * Py
        // (Ay * Bx - Ax * By) * b = Ay * Px - Ax * Py
        // b = Ay * Px - Ax * Py / (Ay * Bx - Ax * By)
        val b = (A.y * prize.x - A.x * prize.y) / (A.y * B.x - A.x * B.y)

        // Ay * a = Py - (By * b)
        // a = (Py - (By * b)) / Ay
        val a = (prize.y - (B.y * b)) / A.y

        val result = A * a + B * b
        return if (result.x == prize.x && result.y == prize.y) Game(a, b) else null
    }

}

fun String.parseButton(): Coordinate =
    this.parseRegex(Regex("Button [A|B]: X[+]([0-9]+), Y[+]([0-9]+)")) {
        Coordinate(it[0].toLong(), it[1].toLong())
    }

fun String.parsePrize(): Coordinate =
    this.parseRegex(Regex("Prize: X=([0-9]+), Y=([0-9]+)")) {
        Coordinate(it[0].toLong(), it[1].toLong())
    }

fun main() {
    val machines = parseByGroup( readData(Year._2024, 13), "") {
        strings -> Machine(strings[0].parseButton(), strings[1].parseButton(), strings[2].parsePrize())
    }

    println(machines.mapNotNull { it.cheapest() }.sumOf { it.price })
    println(machines.map { Machine(it.A, it.B, it.prize + 10000000000000) }
        .mapNotNull { it.cheapest() }.sumOf { it.price })
}
