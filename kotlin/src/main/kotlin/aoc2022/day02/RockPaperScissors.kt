package aoc2022.day02

import library.Year
import library.readData

fun getScore(you: Int, them: Int): Long =
    if (you == (them + 1) % 3) 6 else (if (you == them) 3 else 0)

fun evalGame(line: String, getYou: (Int, Int) -> Int): Long {
    val split = line.split(" ")

    val them = split[0][0] - 'A'
    val you = getYou(them, split[1][0] - 'X')

    // score:          outcome + shape
    return getScore(you, them) + (you + 1L)
}

val determine: (Int, Int) -> Int
    get() = { o, y -> ((o + (y - 1)) + 3) % 3 }

fun main() {
    val data = readData(Year._2022, "day02.txt")

    println(data.sumOf { evalGame(it) { _, y -> y } })
    println(data.sumOf { evalGame(it, determine) })
}
