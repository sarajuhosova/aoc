package aoc2024

import library.Year
import library.geometry.Coordinate
import library.parseByGroup
import library.parseRegex
import library.readData

class Game(val a: Int, val b: Int) {
    val price = a * 3 + b
}

class Machine(
    private val A: Coordinate,
    private val B: Coordinate,
    private val prize: Coordinate
) {

    fun cheapest(): Game? {
        // A * a + B * b = prize
        val potential = mutableSetOf<Game>()

        val eys = (0..100).toMutableSet()

        for (b in 100 downTo 0) {
            val remainder = prize - B.multiply(b)
            if (remainder.x < 0 || remainder.y < 0) continue
            val a = remainder.divide(A)
            if (a != null) {
                eys.remove(a)
                potential.add(Game(a, b))
            }
        }

        for (a in eys) {
            val remainder = prize - A.multiply(a)
            if (remainder.x < 0 || remainder.y < 0) continue
            val b = remainder.divide(B)
            if (b != null) potential.add(Game(a, b))
        }

        return if (potential.isEmpty()) null else potential.maxBy { it.price }
    }

}

fun String.parseButton(): Coordinate =
    this.parseRegex(Regex("Button [A|B]: X[+]([0-9]+), Y[+]([0-9]+)")) {
        Coordinate(it[0].toInt(), it[1].toInt())
    }

fun String.parsePrize(): Coordinate =
    this.parseRegex(Regex("Prize: X=([0-9]+), Y=([0-9]+)")) {
        Coordinate(it[0].toInt(), it[1].toInt())
    }

fun main() {
    val machines = parseByGroup( readData(Year._2024, 13), "") {
        strings -> Machine(strings[0].parseButton(), strings[1].parseButton(), strings[2].parsePrize())
    }

    println(machines.mapNotNull { it.cheapest() }.sumOf { it.price })
}
