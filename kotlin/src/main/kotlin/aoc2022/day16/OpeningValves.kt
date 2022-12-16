package aoc2022.day16

import library.Year
import library.parseRegex
import library.readData
import java.nio.file.Paths
import java.util.LinkedList

class Valve(
    val flowRate : Int,
    val neighbours: List<String>
)

fun clean(data: String): String =
    data.replace("tunnel leads to valve ", "")
        .replace("tunnels lead to valves ", "")

fun parse(data: List<String>): Map<String, Valve> =
    data.map{ it.split("; ") }
        .map { Pair(
            parseRegex("Valve ([A-Z]+) has flow rate=([0-9]+)", it[0]),
            clean(it[1]).split(", ")
        ) }
        .associate { it.first[0] to Valve(it.first[1].toInt(), it.second) }

fun findLargest(valves: Map<String, Valve>, minutes: Int): List<Pair<Int, Int>> {
    val queue = LinkedList<String>()
    valves["AA"]!!.neighbours
        .sortedByDescending { valves[it]!!.flowRate }
        .forEach { queue.add(it) }

    val opened = mutableSetOf<String>()
    opened.add("AA")

    val result = mutableListOf<Pair<Int, Int>>()
    var mins = minutes - 2

    while (!queue.isEmpty() && mins > 0) {
        val current = valves[queue.poll()]!!

        result.add(Pair(current.flowRate, mins))
        mins -= 2

        current.neighbours
            .sortedByDescending { valves[it]!!.flowRate }
            .filter { opened.contains(it) }
            .forEach {
                queue.add(it)
                opened.add(it)
            }
    }

    return result
}

fun findLargestAlt(valves: Map<String, Valve>, minutes: Int): List<Pair<Int, Int>> {
    val opened = mutableSetOf<String>()
    opened.add("AA")

    var current = "AA"

    val result = mutableListOf<Pair<Int, Int>>()
    var mins = minutes - 2

    while (mins > 0) {
        current = valves[current]!!.neighbours
            .filter { opened.contains(it) }
            .sortedByDescending { valves[it]!!.flowRate }[0]

        result.add(Pair(valves[current]!!.flowRate, mins))
        mins -= 2
    }

    return result
}

fun shortestPath(valves: Map<String, Valve>, start: String): Map<String, Int> {
    val s = valves[start]!!

    val queue = LinkedList<Pair<Valve, Int>>()
    queue.offer(Pair(s, 0))

    val visited = mutableMapOf<String, Int>()
    visited[start] = 0

    while (!queue.isEmpty()) {
        val current = queue.poll()

        for (neighbour in current.first.neighbours) {
            if (!visited.keys.contains(neighbour)) {
                queue.offer(Pair(valves[neighbour]!!, current.second + 1))
                visited[neighbour] = current.second + 1
            }
        }
    }

    return visited
}

fun permutation(valves: List<String>, start: String): List<List<String>> {
    val result = mutableListOf<List<String>>()

    for (valve in valves) {
        permutation(valves.filter { it != valve }, valve)
            .forEach {
                result.add(listOf(start) + it)
            }
    }

    return result
}

fun countSteps(permutation: List<String>, paths: Map<String, Map<String, Int>>): Int {
    var steps = 0
    for (i in (1 until permutation.size)) {
        steps += paths[permutation[i - 1]]!![permutation[i]]!!
    }
    return steps
}

fun findLargestAlt2(valves: Map<String, Valve>, minutes: Int): List<Pair<Int, Int>> {
    val nonZero = valves.keys.filter { valves[it]!!.flowRate > 0 }
    val permutations = permutation(nonZero.filter { it != "AA" }, "AA")
    val shortest = nonZero.associateWith { shortestPath(valves, it) }

    val steps = permutations.associateWith { countSteps(it, shortest) }

    return emptyList()
}

fun main() {
    val valves = parse(readData(Year._2022, "day16.txt"))

    val path = findLargestAlt2(valves, 30)
    println(path.map { it.first * it.second }.sumOf { it.toLong() })
}
