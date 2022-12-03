package aoc2016.day03

import library.Year
import library.readData

data class Triangle(val sides: List<Int>) {
    val a = sides[0]
    val b = sides[1]
    val c = sides[2]

    val isPossible : Boolean = (a + b > c) && (a + c > b) && (b + c > a)
}

data class TriTriangle(val triangles: List<Triangle>) {
    private val a = triangles[0]
    private val b = triangles[1]
    private val c = triangles[2]

    private val aPossible = (a.a + b.a > c.a) && (a.a + c.a > b.a) && (b.a + c.a > a.a)
    private val bPossible = (a.b + b.b > c.b) && (a.b + c.b > b.b) && (b.b + c.b > a.b)
    private val cPossible = (a.c + b.c > c.c) && (a.c + c.c > b.c) && (b.c + c.c > a.c)

    private fun toNum(cond: Boolean): Int = (if (cond) 1 else 0)

    val countPossible : Int = toNum(aPossible) + toNum(bPossible) + toNum(cPossible)
}

fun parse(line: String): List<Int> =
    Regex("([0-9]+) +([0-9]+) +([0-9]+)").find(line)!!.destructured.toList().map { it.toInt() }

fun toTrio() {

}

fun main() {
    val data = readData(Year._2016, "day03.txt")

    val horizontalTriangles = data.map { Triangle(parse(it)) }
    println(horizontalTriangles.count { it.isPossible })

    val verticalTriangles = data

}
