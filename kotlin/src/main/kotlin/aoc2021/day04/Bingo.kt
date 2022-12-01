package aoc2021.day04

import library.Year
import library.head
import library.readData
import library.tail

class Board(values: List<List<Int>>) {
    val marks: List<List<Boolean>> = List(values.size) { List(values[0].size) { false } }
}

fun parseBoard(lines: List<String>): Pair<Board, List<String>> =
     Pair(
        Board(lines.takeWhile { it != "" }
            .map { line -> line.split(" ").map { it.toInt() } }),
        lines.dropWhile { it != "" }.tail()
    )

fun parseBoards(lines: List<String>): List<Board> {
    when (lines.size) {
        0 -> return emptyList()
        else -> {
            val (board, next) = parseBoard(lines)
            return listOf(board) + parseBoards(next)
        }
    }
}

fun parse(lines: List<String>): Pair<List<Int>, List<Board>> =
    Pair(
        lines.head().split(",").map { it.toInt() },
        parseBoards(lines.tail().tail())
    )

fun main() {
    val data = readData(Year._2021, "day04_e.txt")

    parse(data)
}