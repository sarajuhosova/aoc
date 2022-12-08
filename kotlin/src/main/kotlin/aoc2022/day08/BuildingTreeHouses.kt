package aoc2022.day08

import library.Year
import library.readData

fun visibleFromUp(grove: List<List<Int>>, i: Int, j: Int): Boolean {
    val tree = grove[i][j]

    for (row in (0 until i)) {
        if (grove[row][j] >= tree) return false
    }
    return true
}

fun visibleFromDown(grove: List<List<Int>>, i: Int, j: Int): Boolean {
    val tree = grove[i][j]

    for (row in (i + 1 until grove.size)) {
        if (grove[row][j] >= tree) return false
    }
    return true
}

fun visibleFromLeft(grove: List<List<Int>>, i: Int, j: Int): Boolean {
    val tree = grove[i][j]

    for (col in (0 until j)) {
        if (grove[i][col] >= tree) return false
    }
    return true
}

fun visibleFromRight(grove: List<List<Int>>, i: Int, j: Int): Boolean {
    val tree = grove[i][j]

    for (col in (j + 1 until grove[0].size)) {
        if (grove[i][col] >= tree) return false
    }
    return true
}

fun isVisible(grove: List<List<Int>>, i: Int, j: Int): Boolean =
    visibleFromLeft(grove, i, j)
            || visibleFromRight(grove, i, j)
            || visibleFromDown(grove, i, j)
            || visibleFromUp(grove, i, j)

fun countUp(grove: List<List<Int>>, i: Int, j: Int): Int {
    val tree = grove[i][j]

    var count = 0
    for (row in (i - 1 downTo 0)) {
        if (grove[row][j] >= tree) return count + 1
        count++
    }
    return count
}

fun countDown(grove: List<List<Int>>, i: Int, j: Int): Int {
    val tree = grove[i][j]

    var count = 0
    for (row in (i + 1 until grove.size)) {
        if (grove[row][j] >= tree) return count + 1
        count++
    }
    return count
}

fun countLeft(grove: List<List<Int>>, i: Int, j: Int): Int {
    val tree = grove[i][j]

    var count = 0
    for (col in (j - 1 downTo 0)) {
        if (grove[i][col] >= tree) return count + 1
        count++
    }
    return count
}

fun countRight(grove: List<List<Int>>, i: Int, j: Int): Int {
    val tree = grove[i][j]

    var count = 0
    for (col in (j + 1 until grove[0].size)) {
        if (grove[i][col] >= tree) return count + 1
        count++
    }
    return count
}

fun calcScenicScore(grove: List<List<Int>>, i: Int, j: Int): Int =
    countUp(grove, i, j) * countDown(grove, i, j) * countLeft(grove, i, j) * countRight(grove, i, j)

data class Grove(
    val data: List<String>
) {
    private val trees: List<List<Int>> = data.map { it.map { c -> c.digitToInt() } }
    val visible: List<List<Boolean>> = trees.mapIndexed { i, line ->
        List(line.size) { j -> isVisible(trees, i, j) }
    }

    val scenicScores: List<List<Int>> = trees.mapIndexed { i, line ->
        List(line.size) { j -> calcScenicScore(trees, i, j) }
    }
}

fun Grove.countVisible() = visible.sumOf { it.count { b -> b } }
fun Grove.getBestScenic() = scenicScores.maxOf { it.max() }

fun main() {
    val grove = Grove(readData(Year._2022, "day08.txt"))

    println(grove.countVisible())
    println(grove.getBestScenic())
}