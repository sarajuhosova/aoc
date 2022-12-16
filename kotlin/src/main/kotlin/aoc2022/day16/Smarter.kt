package aoc2022.day16

import kotlin.math.max
import kotlin.math.min

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

fun calculate(
    valves: Map<String, Valve>,
    paths: Map<String, Map<String, Int>>,
    minute: Int,
    pred: Pair<List<String>, List<String>>,
    it: Pair<MutableList<String>, MutableList<String>>
): Int =
    calculateFlow(valves, countSteps(pred.first + it.first, paths, minute)) + calculateFlow(valves, countSteps(pred.second + it.second, paths, minute))

fun possibilitiesWithElephant(
    pred: Pair<List<String>, List<String>>,
    valves: Map<String, Valve>,
    relevant: List<String>,
    location: Pair<String, String>,
    toGo: Pair<Int, Int>,
    paths: Map<String, Map<String, Int>>,
    startMinute: Int,
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
                pred, valves, relevant, location, Pair(toGo.first - 1, toGo.second - 1), paths, startMinute, minute - 1, min
            ).forEach {
                result.add(it)
            }
        } else {
            for (r in relevant) {
                val time = paths[location.second]!![r]!!
                var possibilities =possibilitiesWithElephant(
                    Pair(pred.first, listOf(r) + pred.second),
                    valves,
                    relevant.filter { it != r },
                    Pair(location.first, r),
                    Pair(toGo.first - 1, time),
                    paths,
                    startMinute,
                    minute - 1,
                    min - ((minute - time) * valves[r]!!.flowRate)
                )
                if (possibilities.isEmpty()) {
                    possibilities = listOf(Pair(mutableListOf(), mutableListOf()))
                }
                possibilities.forEach {
                    it.second.add(0, r)
                }
                val max = possibilities.maxBy { calculate(valves, paths, startMinute, pred, it) }
                result.add(max)
            }
        }
    } else {
        if (toGo.second > 0) {
            for (r in relevant) {
                val time = paths[location.first]!![r]!!

                var possibilities = possibilitiesWithElephant(
                    Pair(listOf(r) + pred.first, pred.second),
                    valves,
                    relevant.filter { it != r },
                    Pair(r, location.second),
                    Pair(time, toGo.second - 1),
                    paths,
                    startMinute,
                    minute - 1,
                    min - ((minute - time) * valves[r]!!.flowRate)
                )
                if (possibilities.isEmpty()) {
                    possibilities = listOf(Pair(mutableListOf(), mutableListOf()))
                }
                possibilities.forEach {
                    it.first.add(0, r)
                }
                val max = possibilities.maxBy { calculate(valves, paths, startMinute, pred, it) }
                result.add(max)
            }
        } else {
            for (r1 in relevant) {
                for (r2 in relevant) {
                    if (r1 == r2) continue
                    val time1 = paths[location.first]!![r1]!!
                    val time2 = paths[location.first]!![r2]!!

                    var possibilities = possibilitiesWithElephant(
                        Pair(listOf(r1) + pred.first, listOf(r2) + pred.second),
                        valves,
                        relevant.filter { it != r1 && it != r2 },
                        Pair(r1, r2),
                        Pair(time1, time2),
                        paths,
                        startMinute,
                        minute - 1,
                        min - ((minute - max(time1, time2)) * (valves[r1]!!.flowRate + valves[r2]!!.flowRate))
                    )
                    if (possibilities.isEmpty()) {
                        possibilities = listOf(Pair(mutableListOf(), mutableListOf()))
                    }
                    possibilities.forEach {
                        it.first.add(0, r1)
                        it.second.add(0, r2)
                    }
                    val max = possibilities.maxBy { calculate(valves, paths, startMinute, pred, it) }
                    result.add(max)
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
        Pair(listOf(initial), listOf(initial)),
        valves,
        nonZero,
        Pair(initial, initial),
        Pair(0, 0),
        shortest,
        minutes,
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
