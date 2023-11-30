package aoc2020.day04

import library.Year
import library.parseByGroup
import library.readData

typealias Passport = Map<String, String>

val fields = mapOf<String, (String) -> Boolean>(
    Pair("byr") { s -> s.toInt() in 1920..2002 },
    Pair("iyr") { s -> s.toInt() in 2010..2020 },
    Pair("eyr") { s -> s.toInt() in 2020..2030 },
    Pair("hgt") { s ->
        val match = Regex("([0-9]+)(in|cm)").find(s)!!.destructured.toList()
        if (match[1] == "cm") match[0].toInt() in 150..193 else match[0].toInt() in 59..76
    },
    Pair("hcl") { s -> Regex("#[0-9a-z]+").matches(s) },
    Pair("ecl") { s -> arrayOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s) },
    Pair("pid") { s -> s.length == 9 && Regex("[0-9]+").matches(s) }
)

fun part1(data: List<Passport>): Int = data.count { passport -> fields.keys.all { passport.contains(it) } }

fun part2(data: List<Passport>): Int =
    data.count { passport -> fields.keys.all {
        passport.contains(it)
    } && passport.keys.all { fields[it]!!(passport[it]!!) } }

fun main() {
    val data = readData(Year._2020, 4).parseByGroup { data ->
        data.flatMap { it.split(" ") }
            .map { it.split(":") }
            .associate { it[0] to it[1] }
    }

    println(part1(data))
    println(part2(data))
}
