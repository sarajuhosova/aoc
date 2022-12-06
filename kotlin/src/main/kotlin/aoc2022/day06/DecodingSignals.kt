package aoc2022.day06

import library.Year
import library.readData

fun findFst(string: String, length: Int): Int =
    (length until string.length).find {
        string.drop(it - length).take(length).toSet().size == length
    } ?: -1


fun main() {
    val data = readData(Year._2022, "day06.txt")[0]

    println(findFst(data, 4))
    println(findFst(data, 14))
}
