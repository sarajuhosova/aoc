package aoc2024

import library.Year
import library.geometry.Direction
import library.geometry.ints.Coordinate
import library.geometry.ints.draw
import library.readData
import library.removeFirst

open class LargeState(
    private var robot: Coordinate,
    private val boxes: MutableSet<Coordinate>,
    private val walls: Set<Coordinate>
) {
    private val restBoxes = boxes.map { Coordinate(it.x, it.y + 1) }.toMutableSet()

    fun move(direction: Direction) {
        val next = robot.move(direction)
        if (next in walls) return
        val horizontal = direction.vertical() // swapped because yeah
        if (next in boxes || next in restBoxes) {
            var position = next
            do {
                position = position.move(direction)
                if (horizontal) position = position.move(direction)
            } while (position in boxes || position in restBoxes)
            if (position in walls) return
            else {
                if (next in boxes) {
                    boxes.remove(next)
                    restBoxes.remove(Coordinate(next.x, next.y + 1))
                } else {
                    restBoxes.remove(next)
                    boxes.remove(Coordinate(next.x, next.y - 1))
                }

                if (!horizontal || direction == Direction.UP) {
                    boxes.add(position)
                    restBoxes.add(Coordinate(position.x, position.y + 1))
                } else {
                    restBoxes.add(position)
                    boxes.add(Coordinate(position.x, position.y - 1))
                }
            }
        }
        robot = next
    }

    fun toMap(): Map<Coordinate, Char> {
        val map = mutableMapOf<Coordinate, Char>()
        map[robot] = '@'
        map.putAll(boxes.associateWith { '[' })
        map.putAll(boxes.map { Coordinate(it.x, it.y + 1) }.associateWith { ']' })
        map.putAll(walls.associateWith { '#' })
        return map
    }

    fun gps(): Long = boxes.sumOf { 100L * it.x + it.y }
}

fun parseLargeMap(input: List<String>): LargeState {
    var robot: Coordinate? = null
    val boxes = mutableSetOf<Coordinate>()
    val walls = mutableSetOf<Coordinate>()

    for (row in input.indices) {
        for (col in input[row].indices) {
            val coordinate = Coordinate(row, col * 2)
            when (input[row][col]) {
                '#' -> {
                    walls.add(coordinate)
                    walls.add(Coordinate(coordinate.x, coordinate.y + 1))
                }
                'O' -> boxes.add(coordinate)
                '@' -> robot = coordinate
            }
        }
    }

    return LargeState(robot!!, boxes, walls)
}

fun parseLargeMoves(input: List<String>): List<Direction> =
    input.joinToString("").map { when (it) {
        '^' -> Direction.UP
        '<' -> Direction.LEFT
        '>' -> Direction.RIGHT
        else -> Direction.DOWN
    }.counterClockwise() }

fun main() {
    val data = readData(Year._2024, 15, "e")
    val first = data.takeWhile { it.isNotBlank() }
    val state = parseLargeMap(first)
    val moves = parseLargeMoves(data.drop(first.size + 1))

    for (direction in moves) {
        println(state.toMap().draw(' '))
        state.move(direction)
    }

    println(state.toMap().draw(' '))
    println(state.gps())
}
