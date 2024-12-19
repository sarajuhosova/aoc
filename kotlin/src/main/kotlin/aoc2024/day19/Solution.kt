package aoc2024.day19

import library.Year
import library.readData

class Towels(private val terminal: Boolean): HashMap<Char, Towels>() {

    private val memory = mutableMapOf<String, Long>()

    fun build(design: String, towels: Towels = this): Long {
        if (design.isEmpty()) return if (towels.terminal) 1 else 0

        var solutions = 0L

        if (towels.isNotEmpty()) {
            val rest = design.drop(1)
            val next = towels[design[0]]
            if (next != null) solutions += this.build(rest, next)
        }

        if (towels.terminal) {
            if (design in memory) {
                solutions += memory[design]!!
            } else {
                solutions += this.build(design)
                memory[design] = solutions
            }
        }

        return solutions
    }

    private fun options(): List<String> =
        (if (terminal) listOf("") else emptyList()) +
                this.flatMap { (k, v) -> v.options().map { k + it } }

    override fun toString(): String = options().filter { it.isNotEmpty() }.joinToString(", ")

}

fun List<String>.toStripes(terminal: Boolean = false): Towels {
    if (this.isEmpty()) return Towels(true)

    val result = Towels(terminal)
    for ((char, strings) in this.groupBy { it[0] }) {
        val (empty, rest) = strings.map { it.drop(1) }.partition { it.isEmpty() }
        result[char] = rest.toStripes(empty.isNotEmpty())
    }
    return result
}

fun main() {
    val data = readData(Year._2024, 19)
    val towels = data.first().split(", ").toStripes()
    val designs = data.drop(2)

    val counts = designs.map { towels.build(it) }.filter { it > 0 }
    println(counts.size)
    println(counts.sum())
}
