package aoc2022.day02

import library.Year
import library.readData
import java.lang.IllegalArgumentException

fun evalGame(line: String, getYou: (Int, Int) -> Int): Long {
    val split = line.split(" ")

    val opponent = split[0][0] - 'A'
    val you = getYou(opponent, split[1][0] - 'X')

    return (if (you == (opponent + 1) % 3) 6 else (if (you == opponent) 3 else 0)) + you + 1L
}

val determine: (Int, Int) -> Int
    get() = { o, y ->
        when (y) {
            0 -> (o - 1 + 3) % 3
            1 -> o
            2 -> (o + 1) % 3
            else -> throw IllegalArgumentException()
        }
    }

fun main() {
    val data = readData(Year._2022, "day02.txt")

    println(data.sumOf { evalGame(it) { _, y -> y } })
    println(data.sumOf { evalGame(it, determine) })
}
