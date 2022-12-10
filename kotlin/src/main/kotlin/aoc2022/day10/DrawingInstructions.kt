package aoc2022.day10

import library.Year
import library.readData

fun parse(data: List<String>): List<Pair<Int, Int>> =
    data.map { it.split(" ") }
        .map { when (it[0]) {
            "addx" -> Pair(it[1].toInt(), 2)
            "noop" -> Pair(0, 1)
            else -> throw IllegalArgumentException()
        } }

fun execute(instructions: List<Pair<Int, Int>>): List<Pair<Long, Int>> {
    val progress = mutableListOf<Pair<Long, Int>>()
    progress.add(Pair(1L, 0))

    for (instr in instructions) {
        val last = progress.last()
        var cycle = last.second
        (1 until instr.second).forEach { _ ->
            progress.add(Pair(last.first, cycle + 1))
            cycle++
        }
        progress.add(Pair(last.first + instr.first, cycle + 1))
    }

    return progress
}

fun getRelevant(progress: List<Pair<Long, Int>>): Long {
    return progress.filter { (it.second + 1) % 40 == 20 }
        .sumOf { state -> state.first * (state.second + 1) }
}

fun atSprite(index: Int, register: Long) =
    register - 1 <= index && index <= register + 1

fun draw(progress: List<Pair<Long, Int>>) {
    for (state in progress) {
        val index = state.second % 40
        print(if (atSprite(index, state.first)) "#" else ".")
        if (index % 40 == 39) {
            println()
        }
    }
}

fun main() {
    val instructions = parse(readData(Year._2022, "day10.txt"))
    val progress = execute(instructions)

    println(getRelevant(progress))
    draw(progress)
}
