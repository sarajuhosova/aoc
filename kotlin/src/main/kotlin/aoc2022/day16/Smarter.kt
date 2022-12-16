package aoc2022.day16

import kotlin.math.max

fun possibilities(
    relevant: List<String>,
    start: String,
    paths: Map<String, Map<String, Int>>,
    minutes: Int
): List<MutableList<String>> {
    if (minutes <= 0 || relevant.isEmpty()) return listOf(mutableListOf(start))

    val result = mutableListOf<MutableList<String>>()

    for (valve in relevant) {
        possibilities(
            relevant.filter { it != valve },
            valve,
            paths,
            minutes - (paths[start]!![valve]!! + 1)
        ).forEach {
            it.add(0, start)
            result.add(it)
        }
    }

    return result
}

fun findLargestSmarter(valves: Map<String, Valve>, minutes: Int): Int {
    val nonZero = valves.keys.filter { valves[it]!!.flowRate > 0 }
    val shortest = nonZero.plus(initial).associateWith { shortestPath(valves, it) }

    val possibilities = possibilities(nonZero, initial, shortest, minutes)
    val steps = possibilities.map { countSteps(it, shortest, minutes) }
    val flows = steps.map { calculateFlow(valves, it) }

    return flows.max()
}

fun canReach(
    valves: Map<String, Valve>,
    relevant: List<String>,
    minute: Int,
    min: Int
): Boolean =
    (0 until relevant.size / 2)
        .sumOf {
            (minute - (it * 3)) * (valves[relevant[it * 2]]!!.flowRate + valves[relevant[it * 2 + 1]]!!.flowRate)
        } > min

fun possibilitiesWithElephant(
    valves: Map<String, Valve>,
    relevant: List<String>,
    location: Pair<String, String>,
    toGo: Pair<Int, Int>,
    paths: Map<String, Map<String, Int>>,
    minute: Int,
    min: Int
): List<Pair<MutableList<String>, MutableList<String>>> {
    if (!canReach(valves, relevant, minute, min) || minute <= 0 || relevant.isEmpty()) {
        return listOf(Pair(mutableListOf(), mutableListOf()))
    }

    val result = mutableListOf<Pair<MutableList<String>, MutableList<String>>>()

    if (toGo.first > 0) {
        if (toGo.second > 0) {
            possibilitiesWithElephant(
                valves, relevant, location, Pair(toGo.first - 1, toGo.second - 1), paths, minute - 1, min
            ).forEach {
                result.add(it)
            }
        } else {
            for (r in relevant) {
                val time = paths[location.second]!![r]!!

                possibilitiesWithElephant(
                    valves,
                    relevant.filter { it != r },
                    Pair(location.first, r),
                    Pair(toGo.first - 1, time),
                    paths,
                    minute - 1,
                    min - ((minute - time) * valves[r]!!.flowRate)
                ).forEach {
                    it.second.add(0, r)
                    result.add(it)
                }
            }
        }
    } else {
        if (toGo.second > 0) {
            for (r in relevant) {
                val time = paths[location.first]!![r]!!

                possibilitiesWithElephant(
                    valves,
                    relevant.filter { it != r },
                    Pair(r, location.second),
                    Pair(time, toGo.second - 1),
                    paths,
                    minute - 1,
                    min - ((minute - time) * valves[r]!!.flowRate)
                ).forEach {
                    it.first.add(0, r)
                    result.add(it)
                }
            }
        } else {
            for (r1 in relevant) {
                for (r2 in relevant) {
                    if (r1 == r2) continue
                    val time1 = paths[location.first]!![r1]!!
                    val time2 = paths[location.first]!![r2]!!

                    possibilitiesWithElephant(
                        valves,
                        relevant.filter { it != r1 && it != r2 },
                        Pair(r1, r2),
                        Pair(time1, time2),
                        paths,
                        minute - 1,
                        min - ((minute - max(time1, time2)) * (valves[r1]!!.flowRate + valves[r2]!!.flowRate))
                    ).forEach {
                        it.first.add(0, r1)
                        it.second.add(0, r2)
                        result.add(it)
                    }
                }
            }
        }
    }

    return result
}

fun findLargestSmarterWithElephant(
    valves: Map<String, Valve>, minutes: Int, min: Int
): Int {
    val nonZero = valves.keys.filter { valves[it]!!.flowRate > 0 }
        .sortedByDescending { valves[it]!!.flowRate }
    val shortest = nonZero.plus(initial).associateWith { shortestPath(valves, it) }

    val possibilities = possibilitiesWithElephant(
        valves,
        nonZero,
        Pair("AA", "AA"),
        Pair(0, 0),
        shortest,
        minutes,
        min
    )
    possibilities.forEach {
        it.first.add(0, initial)
        it.second.add(0, initial)
    }
    val steps = possibilities.map { Pair(
        countSteps(it.first, shortest, minutes),
        countSteps(it.second, shortest, minutes)
    ) }
    val flows = steps.map { calculateFlow(valves, it.first + it.second) }

    return flows.max()
}
