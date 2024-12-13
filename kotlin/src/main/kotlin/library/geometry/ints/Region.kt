package library.geometry.ints

import library.geometry.Direction

/**
 * Assume that they are all connected to each other.
 */
typealias Region = Set<Coordinate>

fun Region.area(): Int = this.size

fun Region.perimeter(): Int =
    this.sumOf { (it.getNeighbours() - this).size }

fun Region.getBorders(): Map<Coordinate, Set<Direction>> {
    val result = mutableMapOf<Coordinate, Set<Direction>>()
    for (coordinate in this) {
        val set = Direction.entries
            .filter { coordinate.move(it) !in this }
            .toSet()
        if (set.isNotEmpty()) result[coordinate] = set
    }
    return result
}

fun Region.countSides(): Int {
    val borders = this.getBorders()
        .mapValues { it.value.toMutableSet() }
        .toMutableMap()

    fun remove(c: Coordinate, d: Direction) {
        val set = borders[c]!!
        set.remove(d)
        if (set.isEmpty()) borders.remove(c)
    }

    fun trace(first: Coordinate, dir: Direction): Int {
        fun back(coordinate: Coordinate, direction: Direction): Boolean =
            coordinate == first && direction == dir

        var current = Pair(first, dir)
        var total = 0
        outer@ do {
            val edges = borders[current.first]!!
            remove(current.first, current.second)
            var nextDir = current.second.clockwise()

            while (nextDir in edges || back(current.first, nextDir)) {
                total++
                if (back(current.first, nextDir)) break@outer
                remove(current.first, nextDir)
                nextDir = nextDir.clockwise()
            }

            var next = current.first.move(nextDir)
            nextDir = nextDir.counterClockwise()

            if (back(next, nextDir)) break@outer

            if (next !in borders || nextDir !in borders[next]!!) {
                next = next.move(nextDir)
                nextDir = nextDir.counterClockwise()
                total++
                if (back(next, nextDir)) break@outer
            }

            current = Pair(next, nextDir)
        } while (true)
        return total
    }

    var total = 0
    while (borders.isNotEmpty()) {
        val first = borders.keys.first()
        total += trace(first, borders[first]!!.first())
    }
    return total
}
