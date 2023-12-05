package aoc2023.day05

import library.*
import java.util.Optional

data class Entry(
    val source: Long,
    val destination: Long,
    val range: Long
) {
    fun get(key: Long): Optional<Long> {
        if (key in (source) until (source + range)) {
            return Optional.of(destination + (key - source))
        }
        return Optional.empty()
    }

    fun getMaxRange(key: Long): Optional<Long> {
        if (key in (source) until (source + range)) {
            return Optional.of(source + range - key)
        }
        return Optional.empty()
    }
}

data class Mapper(
    val source: String,
    val destination: String,
    val entries: List<Entry>
) {
    private val sortedEntries = entries.sortedBy { it.source }

    fun get(key: Long): Long {
        for (entry in entries) {
            val maybe = entry.get(key)
            if (maybe.isPresent) {
                return maybe.get()
            }
        }
        return key
    }

    fun getMaxRange(key: Long): Long {
        for (entry in entries) {
            val maybe = entry.getMaxRange(key)
            if (maybe.isPresent) {
                return maybe.get()
            }
        }
        val next = sortedEntries.find { it.source > key }

        return (next?.source ?: Long.MAX_VALUE) - key
    }
}

fun parse(input: List<String>): Mapper {
    val title = parseRegex("([a-z]+)\\-to\\-([a-z]+) map:", input.head())
    val entries = input.tail()
        .map { it.split(" ") }
        .map { Entry(it[1].toLong(), it[0].toLong(), it[2].toLong()) }
    return Mapper(title[0], title[1], entries)
}

fun findSeedLocation(seed: Long, mappers: List<Mapper>): Long {
    var result = seed
    for (mapper in mappers) {
        result = mapper.get(result)
    }
    return result
}

fun findSeedLocations(seeds: List<Long>, mappers: List<Mapper>): Long =
    seeds.minOf { findSeedLocation(it,  mappers) }

fun findNextInRange(lower: Long, mappers: List<Mapper>): Long {
    var upper = Long.MAX_VALUE
    var key = lower

    for (mapper in mappers) {
        val nextUpper = mapper.getMaxRange(key)
        key = mapper.get(key)
        if (nextUpper < upper) upper = nextUpper
    }

    return upper
}

fun findSeedRangeMinimum(lower: Long, upper: Long, mappers: List<Mapper>): Long {
    var min = Long.MAX_VALUE
    var next = lower

    while (next <= upper) {
        val value = findSeedLocation(next, mappers)
        if (value < min) min = value

        next += findNextInRange(next, mappers)
    }

    return min
}

fun findSeedRangeLocations(seeds: List<Long>, mappers: List<Mapper>): Long {
    var min = Long.MAX_VALUE
    for (i in seeds.indices step 2) {
        val rangeMin = findSeedRangeMinimum(
            seeds[i], seeds[i] + seeds[i +1], mappers
        )
        if (rangeMin < min) min = rangeMin
    }
    return min
}

fun main() {
    val data = readData(Year._2023, 5)

    val seeds = data[0].split(": ")[1]
        .split(" ")
        .map { it.toLong() }
    val mappers = data.drop(2).parseByGroup { parse(it) }

    println(findSeedLocations(seeds, mappers))
    println(findSeedRangeLocations(seeds, mappers))
}
