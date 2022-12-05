package aoc2022.day05

import library.Year
import library.parseRegex
import library.readData
import library.tail
import java.util.*

data class Move(
    val amount: Int,
    val from: Int,
    val to: Int
)

fun parseMoves(data: List<String>): List<Move> =
    data.parseRegex(
        "move ([0-9]+) from ([0-9]+) to ([0-9]+)"
    ) { Move(it[0].toInt(), it[1].toInt() - 1, it[2].toInt() - 1) }

fun parseCurrent(data: List<String>): List<Stack<Char>> {
    val size = data.last().split("   ").size
    val stack = (0 until size).map { Stack<Char>() }

    for (line in data.dropLast(1).reversed()) {
        for (i in (0 until size)) {
            val index = (i * 4) + 1
            if (index >= line.length) break

            val c = line[index]
            if (c != ' ') {
                stack[i].push(c)
            }
        }
    }

    return stack
}

fun execute9000(crates: List<Stack<Char>>, moves: List<Move>): List<Stack<Char>> {
    moves.forEach {
        (0 until it.amount)
            .forEach { _ -> crates[it.to].push(crates[it.from].pop()) }
    }
    return crates
}

fun execute9001(crates: List<Stack<Char>>, moves: List<Move>): List<Stack<Char>> {
    moves.forEach {
        (0 until it.amount)
            .map { _ -> crates[it.from].pop() }
            .reversed()
            .map { c -> crates[it.to].push(c) }
    }
    return crates
}

fun getTops(crates: List<Stack<Char>>) =
    crates.fold("") { acc, stack -> acc + stack.peek() }

fun main() {
    val data = readData(Year._2022, "day05.txt")

    val crateData = data.takeWhile { it != "" }
    val moves = parseMoves(data.dropWhile { it != "" }.tail())

    println(getTops(execute9000(parseCurrent(crateData), moves)))
    println(getTops(execute9001(parseCurrent(crateData), moves)))
}
