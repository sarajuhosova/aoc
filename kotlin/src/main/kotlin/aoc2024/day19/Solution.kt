package aoc2024.day19

import library.Year
import library.readData
import library.tail

class Stripe(val char: Char, val next: Set<Stripe>, val terminal: Boolean) {

    private fun options(): Set<String> =
        next.flatMap { it.options().map { s -> char + s } }.toSet()

    override fun toString(): String = "{${options().joinToString(", ")}}"

}

typealias Towels = Set<Stripe>

fun Towels.get(char: Char): Stripe? = this.find { it.char == char }

fun Towels.build(design: String, next: Set<Stripe> = this): Boolean {
    if (design.isEmpty()) return true
    if (next.isEmpty()) return false

    val first = next.get(design[0]) ?: return false
    if (first.terminal && this.build(design.drop(1))) return true
    return this.build(design.drop(1), first.next)
}

fun List<String>.toStripes(): Towels {
    if (this.isEmpty()) return emptySet()
    val result = mutableSetOf<Stripe>()
    for ((char, strings) in this.filter { it.isNotEmpty() }.groupBy { it[0] }) {
        val next = strings.map { it.drop(1) }.toStripes()
        result.add(Stripe(char, next, strings.any { it.length == 1 }))
    }
    return result
}

fun main() {
    val data = readData(Year._2024, 19)
    val towels = data.first().split(", ").toStripes()
    val designs = data.drop(2)

    println(designs.count { towels.build(it) })
}
