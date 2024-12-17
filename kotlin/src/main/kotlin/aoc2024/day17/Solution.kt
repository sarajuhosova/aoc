package aoc2024.day17

import library.*

typealias Program = List<Byte>

var logger = mutableListOf<String>()

fun Program.run(init: Triple<Long, Long, Long>): List<Byte> {
    val out = mutableListOf<Byte>()
    var (a, b, c) = init

    fun getCombo(position: Int): Long = when (this[position]) {
        in 0..3 -> this[position].toLong()
        4.toByte() -> a
        5.toByte() -> b
        6.toByte() -> c
        else -> throw Exception("Unknown combo operand")
    }

    fun getComboLog(position: Int): String = when (this[position]) {
        in 0..3 -> this[position].toString()
        4.toByte() -> "a"
        5.toByte() -> "b"
        6.toByte() -> "c"
        else -> throw Exception("Unknown combo operand")
    }

    var pointer = 0
    while (pointer in indices) {
        when (this[pointer]) {
            0.toByte() -> {
                a /= pow(2, getCombo(pointer + 1))
                logger.add("a = a / (2^${getComboLog(pointer + 1)})    ($a)")
            }
            1.toByte() -> {
                b = b xor this[pointer + 1].toLong()
                logger.add("b = b xor ${this[pointer + 1]}    ($b)")
            }
            2.toByte() -> {
                b = getCombo(pointer + 1) % 8
                logger.add("b = ${getComboLog(pointer + 1)} % 8    ($b)")
            }
            3.toByte() -> if (a != 0L) {
                pointer = this[pointer + 1].toInt() - 2
                logger.add("jump     (${pointer + 2})")
            } else logger.add("no jump")
            4.toByte() -> {
                b = b xor c
                logger.add("b = b xor c    ($b)")
            }
            5.toByte() -> {
                val output = (getCombo(pointer + 1) % 8).toByte()
                out.add(output)
                logger.add(">> ${getComboLog(pointer + 1)} % 8     ($output)")
            }
            6.toByte() -> {
                b = a / pow(2, getCombo(pointer + 1))
                logger.add("b = a / (2^${getComboLog(pointer + 1)})    ($b)")
            }
            7.toByte() -> {
                c = a / pow(2, getCombo(pointer + 1))
                logger.add("c = a / (2^${getComboLog(pointer + 1)})    ($c)")
            }
        }
        pointer += 2
    }

    return out
}

fun List<Byte>.computeAs(): Set<Long> {
    if (this.isEmpty()) return setOf(0L)

    val target = this.head()
    val previousAs = this.tail().computeAs()

    val result = mutableSetOf<Long>()

    for (thisA in 0..7) {
        for (previousA in previousAs) {
            val a: Long = (previousA shl 3) + thisA
            // b = a % 8
            // b = b xor 1
            val bShift: Int = thisA xor 1
            // c = a / (2^b)
            val c = a shr bShift
            // a = a / (2^3)
            // b = b xor c
            var b = bShift.toLong() xor c
            // b = b xor 6
            b = b xor 6
            // >> b % 8
            if ((b % 8).toByte() == target) result.add(a)
        }
    }

    return result
}

fun String.parseRegister(): Long = this.split(": ")[1].toLong()

fun List<String>.parse(): Pair<Triple<Long, Long, Long>, Program> {
    return Pair(
        Triple(this[0].parseRegister(), this[1].parseRegister(), this[2].parseRegister()),
        this[4].split(": ")[1].split(",").map { it.toByte() }
    )
}

fun main() {
    val (registers, program) = readData(Year._2024, 17).parse()

    println(program.run(registers).joinToString(","))

    java.io.File("src/main/resources/aoc2024/day17/log.txt")
        .writeText(logger.joinToString("\n"))

    println(program.computeAs().minOrNull() ?: "No potential seed found :(")
}
