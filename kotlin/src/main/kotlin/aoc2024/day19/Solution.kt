package aoc2024.day19

import library.Year
import library.readData

class Towels(private val terminal: Boolean): HashMap<Char, Towels>() {

    private val memory = mutableMapOf<String, Long>()

    fun build(design: String, towels: Towels = this): Long {
        // if suffix is empty, return 1 if we have used a full towel, else 0
        if (design.isEmpty()) return if (towels.terminal) 1 else 0

        var solutions = 0L

        // if we are not at the end of a state machine
        // (i.e. there is a longer string potentially still)
        if (towels.isNotEmpty()) {
            // consume the next character
            val rest = design.drop(1)
            // advance to the next state along the edge of that next character
            val next = towels[design[0]]
            // if there is such a next state, continue building this
            // else there are no solutions here
            if (next != null) solutions += this.build(rest, next)
        }

        // if we are at a terminal
        // we can also potentially start a new towel
        if (towels.terminal) {
            // if we have calculated the rest of this design already
            if (design in memory) {
                // just return that
                solutions += memory[design]!!
            } else {
                // else calculate this design starting from a new towel
                solutions += this.build(design)
                // store that calculation
                memory[design] = solutions
            }
        }

        return solutions
    }

    private fun options(): List<String> =
        (if (terminal) listOf("") else emptyList()) +
                this.flatMap { (k, v) -> v.options().map { k + it } }

    override fun toString(): String = options().joinToString(", ")

}

fun List<String>.toTowels(terminal: Boolean = false): Towels {
    if (this.isEmpty()) return Towels(terminal)

    val result = Towels(terminal)
    for ((char, strings) in this.groupBy { it[0] }) {
        val (empty, rest) = strings.map { it.drop(1) }.partition { it.isEmpty() }
        result[char] = rest.toTowels(empty.isNotEmpty())
    }
    return result
}

fun main() {
    val data = readData(Year._2024, 19)
    val towels = data.first().split(", ").toTowels()
    val designs = data.drop(2)

    val counts = designs.map { towels.build(it) }.filter { it > 0 }
    println(counts.size)
    println(counts.sum())

    println(towels.toString().split(", ").sorted().joinToString(", "))
    println(data[0].split(", ").sorted().joinToString(", "))
}
