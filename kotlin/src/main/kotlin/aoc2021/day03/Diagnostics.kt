package aoc2021.day03

import library.Year
import library.readData
import library.stringify

val mostCommonBit: (List<String>, Int) -> Char
    get() = { data, index ->
        if (data.count { s -> s[index] == '0' } > (data.size / 2)) '0' else '1'
    }

fun flip(chr: Char): Char =
    if (chr == '0') '1' else '0'

fun flip(str: String): String =
    str.map { flip(it) }.stringify()

fun getRates(data: List<String>): Pair<Int, Int> {
    val s: String = (0 until data[0].length)
        .map { mostCommonBit(data, it) }.stringify()

    return Pair(s.toInt(2), flip(s).toInt(2))
}

fun filter(
    data: List<String>,
    getBit: (List<String>, Int) -> Char,
    index: Int = 0
): Int =
    when (data.size) {
        1 -> data[0].toInt(2)
        else -> filter(
            (getBit(data, index))
                .let { b ->
                    data.filter { it[index] == b }
                },
            getBit,
            index + 1
        )
    }

fun main() {
    val data = readData(Year._2021, "day03.txt")

    val rates = getRates(data)
    println(rates.first * rates.second)

    val oxygen = filter(data, mostCommonBit)
    val co2 = filter(data, { d, i -> flip(mostCommonBit(d, i)) })
    println(oxygen * co2)
}