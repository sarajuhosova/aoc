package aoc2016.day02

import library.Year
import library.readData
import library.tail

fun getKey(comb: String, init: Key): Key =
    comb.toCharArray().fold(init) { acc, c -> Move.valueOf(c.toString()).move(acc) }

val appendNextKey: (List<Key>, String) -> List<Key>
    get() = { keys: List<Key>, s ->
        keys + getKey(s, keys.last())
    }

fun getCode(combs: List<String>): String =
    combs.fold(listOf(Key()), appendNextKey)
        .tail()
        .map { it.value.toString() }
        .fold("") { acc, s -> acc + s }

fun main() {
    val data = readData(Year._2016, "day02.txt")

    println(getCode(data))
}