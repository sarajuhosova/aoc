package aoc2022.day16

import java.util.*

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

fun permutation(valves: List<String>, start: String): List<MutableList<String>> {
    if (valves.isEmpty()) return listOf(mutableListOf(start))

    val result = mutableListOf<MutableList<String>>()

    for (valve in valves) {
        permutation(valves.filter { it != valve }, valve)
            .forEach {
                it.add(0, start)
                result.add(it)
            }
    }

    return result
}

fun countSteps(
    permutation: List<String>, paths: Map<String, Map<String, Int>>, minutes: Int
): List<Pair<String, Int>> {
    val result = mutableListOf<Pair<String, Int>>()

    var mins = minutes
    for (i in (1 until permutation.size)) {
        mins -= (paths[permutation[i - 1]]!![permutation[i]]!! + 1)
        if (mins <= 0) break
        result.add(Pair(permutation[i], mins))
    }

    return result
}

fun calculateFlow(valves: Map<String, Valve>, path: List<Pair<String, Int>>): Int {
    return path.sumOf { valves[it.first]!!.flowRate * it.second }
}

fun findLargestBruteForce(valves: Map<String, Valve>, minutes: Int): Int {
    val nonZero = valves.keys.filter { valves[it]!!.flowRate > 0 }
    val shortest = nonZero.plus(initial).associateWith { shortestPath(valves, it) }

    return permutation(nonZero.filter { it != initial }, initial).map {
        calculateFlow(valves, countSteps(it, shortest, minutes))
    }.max()
}
