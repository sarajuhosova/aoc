package aoc2022.day18

import aoc2022.day16.Valve
import library.Year
import library.readData
import java.util.*

data class Space(
    val x: Int,
    val y: Int,
    val z: Int
)

fun parse(data: List<String>): List<Space> =
    data.map { it.split(",") }.map { Space(
        it[0].toInt(), it[1].toInt(), it[2].toInt()
    ) }

fun isOpen(droplet: Space, droplets: List<Space>): Int =
    if (droplets.contains(droplet)) 0 else 1

fun countOpenSides(droplets: List<Space>): Int =
    droplets.sumOf {
        listOf(
            isOpen(Space(it.x - 1, it.y, it.z), droplets),
            isOpen(Space(it.x + 1, it.y, it.z), droplets),
            isOpen(Space(it.x, it.y - 1, it.z), droplets),
            isOpen(Space(it.x, it.y + 1, it.z), droplets),
            isOpen(Space(it.x, it.y, it.z - 1), droplets),
            isOpen(Space(it.x, it.y, it.z + 1), droplets)
        ).sum()
    }

fun hasPath(valves: Map<String, Valve>, start: String): Map<String, Int> {
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

fun isOutside(space: Space, droplet: List<Space>): Boolean {
    if (droplet.contains(space)) return false

    val xs = droplet.filter { it.y == space.y && it.z == space.z }.map { it.x }
    val ys = droplet.filter { it.x == space.x && it.z == space.z }.map { it.y }
    val zs = droplet.filter { it.x == space.x && it.y == space.y }.map { it.z }

    return xs.size < 2 || xs.min() > space.x || space.x > xs.max()
            || ys.size < 2 || ys.min() > space.y || space.y > ys.max()
            || zs.size < 2 || zs.min() > space.z || space.z > zs.max()
}

fun isPocket(space: Space, droplets: List<Space>, outside: List<Space>): Boolean {
    val queue = LinkedList<Space>()
    queue.offer(space)

    val visited = mutableSetOf<Space>()
    visited.add(space)

    while (!queue.isEmpty()) {
        val current = queue.poll()

        if (outside.contains(current)) return false
        visited.add(space)

        val xMin = Space(space.x - 1, space.y, space.z)
        if (!visited.contains(xMin) && !droplets.contains(xMin)) queue.offer(xMin)
        val xMax = Space(space.x + 1, space.y, space.z)
        if (!visited.contains(xMax) && !droplets.contains(xMax)) queue.offer(xMax)

        val yMin = Space(space.x, space.y - 1, space.z)
        if (!visited.contains(yMin) && !droplets.contains(yMin)) queue.offer(yMin)
        val yMax = Space(space.x, space.y + 1, space.z)
        if (!visited.contains(yMax) && !droplets.contains(yMax)) queue.offer(yMax)

        val zMin = Space(space.x, space.y, space.z - 1)
        if (!visited.contains(zMin) && !droplets.contains(zMin)) queue.offer(zMin)
        val zMax = Space(space.x, space.y, space.z + 1)
        if (!visited.contains(zMax) && !droplets.contains(zMax)) queue.offer(zMax)
    }

    return true
}

fun findAirPockets(droplets: List<Space>): List<Space> {
    val pockets = mutableListOf<Space>()

    val xs = droplets.map { it.x }
    val ys = droplets.map { it.y }
    val zs = droplets.map { it.z }

    val outside = mutableListOf(Space(xs.min() - 1, ys.min() - 1, zs.min() - 1))

    println("${xs.min()} < x < ${xs.max()}")
    println("${ys.min()} < y < ${ys.max()}")
    println("${zs.min()} < z < ${zs.max()}")
    for (i in (xs.min() + 1 until xs.max())) {
        for (j in (ys.min() + 1 until ys.max())) {
            for (k in (zs.min() + 1 until zs.max())) {
                val space = Space(i, j, k)
                if (!droplets.contains(space)) {
                    if (isPocket(space, droplets, outside)) {
                        pockets.add(space)
                    } else {
                        outside.add(space)
                    }
                }
            }
            println("Row $j done!")
        }
    }

    return pockets
}

fun main() {
    val droplets = parse(readData(Year._2022, "day18.txt"))

    val openSides = countOpenSides(droplets)
    println(openSides)

    val pockets = findAirPockets(droplets)
    println(openSides - countOpenSides(pockets))
}
