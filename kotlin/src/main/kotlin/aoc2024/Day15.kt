package aoc2024

import library.Year
import library.geometry.Direction
import library.geometry.ints.Coordinate
import library.geometry.ints.draw
import library.readData

open class State(
    private var robot: Coordinate,
    private val boxes: MutableSet<Coordinate>,
    private val walls: Set<Coordinate>
) {
    fun copy(): State = State(
        robot.copy(),
        boxes.map { it.copy() }.toMutableSet(),
        walls.map { it.copy() }.toSet()
    )

    fun move(direction: Direction) {
        val next = robot.move(direction)
        if (next in walls) return
        if (next in boxes) {
            var position = next
            while (position in boxes) {
                position = position.move(direction)
            }
            if (position in walls) return
            else {
                boxes.remove(next)
                boxes.add(position)
            }
        }
        robot = next
    }

    fun toMap(): Map<Coordinate, Char> {
        val map = mutableMapOf<Coordinate, Char>()
        map[robot] = '@'
        map.putAll(boxes.associateWith { 'O' })
        map.putAll(walls.associateWith { '#' })
        return map
    }

    fun gps(): Long = boxes.sumOf { 100L * it.x + it.y }
}

fun parseMap(input: List<String>): State {
    var robot: Coordinate? = null
    val boxes = mutableSetOf<Coordinate>()
    val walls = mutableSetOf<Coordinate>()

    for (row in input.indices) {
        for (col in input[row].indices) {
            val coordinate = Coordinate(row, col)
            when (input[row][col]) {
                '#' -> walls.add(coordinate)
                'O' -> boxes.add(coordinate)
                '@' -> robot = coordinate
            }
        }
    }

    return State(robot!!, boxes, walls)
}

fun parseMoves(input: List<String>): List<Direction> =
    input.joinToString("").map { when (it) {
        '^' -> Direction.UP
        '<' -> Direction.LEFT
        '>' -> Direction.RIGHT
        else -> Direction.DOWN
    }.counterClockwise() }

fun main() {
    val data = readData(Year._2024, 15)
    val first = data.takeWhile { it.isNotBlank() }
    val state = parseMap(first)
    val moves = parseMoves(data.drop(first.size + 1))

    val state1 = state.copy()
    for (direction in moves) {
        state1.move(direction)
    }

    println(state1.toMap().draw(' '))
    println(state1.gps())
}
