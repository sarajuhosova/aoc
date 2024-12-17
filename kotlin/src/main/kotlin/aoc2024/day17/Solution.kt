package aoc2024.day17

import library.Year
import library.pow
import library.readData

typealias Program = List<Byte>

fun Program.run(init: Triple<Int, Int, Int>): List<Byte> {
    val out = mutableListOf<Byte>()
    var (a, b, c) = init

    fun getCombo(position: Int): Int = when (this[position]) {
        in 0..3 -> this[position].toInt()
        4.toByte() -> a
        5.toByte() -> b
        6.toByte() -> c
        else -> throw Exception("Unknown combo operand")
    }

    var pointer = 0
    while (pointer in indices) {
        when (this[pointer]) {
            0.toByte() -> a /= pow(2, getCombo(pointer + 1))
            1.toByte() -> b = b xor this[pointer + 1].toInt()
            2.toByte() -> b = getCombo(pointer + 1) % 8
            3.toByte() -> if (a != 0) {
                pointer = this[pointer + 1].toInt() - 2
            }
            4.toByte() -> b = b xor c
            5.toByte() -> out.add((getCombo(pointer + 1) % 8).toByte())
            6.toByte() -> b = a / pow(2, getCombo(pointer + 1))
            7.toByte() -> c = a / pow(2, getCombo(pointer + 1))
        }
        pointer += 2
    }
    return out
}

fun String.parseRegister(): Int = this.split(": ")[1].toInt()

fun List<String>.parse(): Pair<Triple<Int, Int, Int>, Program> {
    return Pair(
        Triple(this[0].parseRegister(), this[1].parseRegister(), this[2].parseRegister()),
        this[4].split(": ")[1].split(",").map { it.toByte() }
    )
}

fun main() {
    val (registers, program) = readData(Year._2024, 17).parse()

    println(program.run(registers).joinToString(","))
}
