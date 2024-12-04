package aoc2016

import library.Year
import library.countAll
import library.readData
import library.stringify

fun List<String>.byMostCommon(): String =
    (0 until this[0].length).map { i ->
        this.map { s -> s[i] }.countAll().maxBy { it.value }.key
    }.stringify()

fun List<String>.byLeastCommon(): String =
    (0 until this[0].length).map { i ->
        this.map { s -> s[i] }.countAll().minBy { it.value }.key
    }.stringify()

fun main() {
    val data = readData(Year._2016, 6)

    println(data.byMostCommon())
    println(data.byLeastCommon())
}
