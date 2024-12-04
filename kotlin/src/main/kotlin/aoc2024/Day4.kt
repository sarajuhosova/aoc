package aoc2024

import library.Coordinate
import library.Direction
import library.Year
import library.readData

fun Map<Coordinate, Char>.get(coordinates: List<Coordinate>): String? {
    var result = ""
    for (coordinate in coordinates) {
        if (coordinate !in this) return null
        result += this[coordinate]!!
    }
    return result
}

fun findAllXMAS(puzzle: Map<Coordinate, Char>): Int {
    val xs = puzzle.filter { it.value == 'X' }.keys

    val potential = mutableSetOf<List<Coordinate>>()
    for (x in xs) {
        potential.add(x.moveSteps(3, Direction.UP))
        potential.add(x.moveSteps(3, Direction.DOWN))
        potential.add(x.moveSteps(3, Direction.LEFT))
        potential.add(x.moveSteps(3, Direction.RIGHT))
        potential.add(x.moveSteps(3, Direction.UP, Direction.LEFT))
        potential.add(x.moveSteps(3, Direction.UP, Direction.RIGHT))
        potential.add(x.moveSteps(3, Direction.DOWN, Direction.LEFT))
        potential.add(x.moveSteps(3, Direction.DOWN, Direction.RIGHT))
    }

    return potential.map { puzzle.get(it) }
        .count { it != null && it == "MAS" }
}

fun Pair<Coordinate, Coordinate>.check(puzzle: Map<Coordinate, Char>): Boolean {
    if (this.first !in puzzle || this.second !in puzzle) return false
    val f = puzzle[this.first]
    val s = puzzle[this.second]
    return (f == 'M' && s == 'S') || (f == 'S' && s == 'M')
}

typealias X_MAS = Pair<Pair<Coordinate, Coordinate>, Pair<Coordinate, Coordinate>>
fun X_MAS.checkX(puzzle: Map<Coordinate, Char>): Boolean =
    this.first.check(puzzle) && this.second.check(puzzle)

fun findAllX_MAS(puzzle: Map<Coordinate, Char>): Int {
    val eys = puzzle.filter { it.value == 'A' }.keys

    val potential = mutableSetOf<X_MAS>()
    for (a in eys) {
        potential.add(Pair(
            Pair(
                a.move(Direction.UP, Direction.LEFT),
                a.move(Direction.DOWN, Direction.RIGHT)
            ),
            Pair(
                a.move(Direction.UP, Direction.RIGHT),
                a.move(Direction.DOWN, Direction.LEFT),
            )
        ))
    }

    return potential.count { it.checkX(puzzle) }
}

// TODO: refactor?
fun main() {
    val data = readData(Year._2024, 4)

    val puzzle = mutableMapOf<Coordinate, Char>()
    for (i in data.indices) {
        for (j in data[i].indices) {
            puzzle[Coordinate(i, j)] = data[i][j]
        }
    }

    println(findAllXMAS(puzzle))
    println(findAllX_MAS(puzzle))
}
