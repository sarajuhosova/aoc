package aoc2018

import library.Year
import library.readData
import java.time.LocalDateTime

val FALLS_ASLEEP = "falls asleep"
val WAKES_UP = "wakes up"

data class Entry(
    val time: LocalDateTime,
    val action: String
) {
    fun getAsStart() = if (time.hour == 0) time.minute else 0
    fun getAsEnd() = if (time.hour == 0) time.minute else 60
}

fun List<Entry>.getGuardDuty(): List<Guard> {
    val guards = mutableMapOf<Int, Array<Int>>()

    var current: Int = -1
    var start = -1
    for (entry in this) {
        when (entry.action) {
            FALLS_ASLEEP -> start = entry.getAsStart()
            WAKES_UP -> {
                val end = entry.getAsEnd()
                for (i in start until end) {
                    val guard = guards[current]!!
                    guard[i] = guard[i] + 1
                }
            }
            else -> {
                val id = entry.action.split(" ")
                    .find { s -> s.contains("#") }!!
                    .substring(1)
                    .toInt()
                if (id !in guards) guards[id] = Array(60) { 0 }
                current = id
            }
        }
    }

    return guards.toList().map { Guard(it.first, it.second) }
}

data class Guard(val id: Int, val asleep: Array<Int>) {
    fun getSleepiestMinute(): Int = asleep.indices.maxBy { asleep[it] }
}

fun List<Guard>.getSleepiestGuard(): Guard = this.maxBy { it.asleep.sum() }

fun main() {
    val log = readData(Year._2018, 4).map {
        Entry(LocalDateTime.parse(it.substring(1, 17).replace(' ', 'T')),
            it.substring(19))
    }.sortedBy { it.time }

    val guards = log.getGuardDuty()

    println(guards.getSleepiestGuard().getSleepiestMinute())
}
