package aoc2022.day12

import library.Year
import library.readData
import java.util.*
import kotlin.math.min

class Node(
    val char: Char,
    val neighbours: MutableList<Node> = mutableListOf(),
    val end: Boolean = false
)

fun parse(data: List<String>): List<List<Node>> {
    val nodes = data.map { it.map { c ->
        Node(if (c == 'S') 'a' else (if (c == 'E') 'z' else c), end = c == 'E')
    } }

    for (i in (data.indices)) {
        for (j in data[0].indices) {
            val n = nodes[i][j]

            if (i - 1 >= 0 && nodes[i - 1][j].char <= n.char + 1) n.neighbours.add(nodes[i - 1][j])
            if (i + 1 < data.size && nodes[i + 1][j].char <= n.char + 1) n.neighbours.add(nodes[i + 1][j])
            if (j - 1 >= 0 && nodes[i][j - 1].char <= n.char + 1) n.neighbours.add(nodes[i][j - 1])
            if (j + 1 < data[0].length && nodes[i][j + 1].char <= n.char + 1) n.neighbours.add(nodes[i][j + 1])
        }
    }
    return nodes
}

fun findStart(data: List<String>): Pair<Int, Int> {
    for (i in (data.indices)) {
        for (j in data[0].indices) {
            if (data[i][j] == 'S') return Pair(i, j)
        }
    }
    return Pair(-1, -1)
}

fun shortestPath(start: Node): Int {
    val queue = LinkedList<Pair<Node, Int>>()
    queue.offer(Pair(start, 0))

    val visited = mutableSetOf<Node>()
    visited.add(start)

    while (!queue.isEmpty()) {
        val current = queue.poll()
        if (current.first.end) return current.second

        for (neighbour in current.first.neighbours) {
            if (!visited.contains(neighbour)) {
                queue.offer(Pair(neighbour, current.second + 1))
                visited.add(neighbour)
            }
        }
    }

    return -1
}

fun findShortest(nodes: List<List<Node>>): Int {
    var shortest = Integer.MAX_VALUE
    for (row in nodes) {
        for (node in row) {
            if (node.char == 'a') {
                val path = shortestPath(node)
                if (path > 0) shortest = min(shortest, path)
            }
        }
    }
    return shortest
}

fun main() {
    val data = readData(Year._2022, "day12.txt")
    val start = findStart(data)

    val nodes = parse(data)
    val begin = nodes[start.first][start.second]

    println(shortestPath(begin))
    println(findShortest(nodes))
}
