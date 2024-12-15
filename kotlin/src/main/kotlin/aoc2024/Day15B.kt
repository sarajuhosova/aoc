package aoc2024

import library.Year
import library.geometry.Direction
import library.geometry.ints.Coordinate
import library.geometry.ints.draw
import library.readData

typealias Box = Pair<Coordinate, Coordinate>

fun Box.move(direction: Direction, steps: Int = 1): Box =
    Pair(this.first.move(direction, steps), this.second.move(direction, steps))
fun Box.gps(): Long = 100L * this.first.x + this.first.y

fun Set<Box>.get(coordinate: Coordinate): Box? =
    this.find { it.first == coordinate || it.second == coordinate }
fun Set<Box>.toCoordinates(): Set<Coordinate> =
    this.flatMap { setOf(it.first, it.second) }.toSet()

open class LargeState(
    private var robot: Coordinate,
    private val boxes: MutableSet<Box>,
    private val walls: Set<Coordinate>
) {

    private fun moveHorizontal(direction: Direction) {
        val next = robot.move(direction)

        val toMove = mutableSetOf<Box>()
        var position = next.copy()
        do {
            if (position in walls) return
            val current = boxes.get(position) ?: break

            toMove.add(current)
            position = position.move(direction, 2)
        } while (true)

        for (box in toMove) boxes.remove(box)
        for (box in toMove) boxes.add(box.move(direction))

        robot = next
    }

    private fun moveVertical(direction: Direction) {
        val toMove = mutableSetOf<Box>()
        val next = robot.move(direction)

        var positions = setOf(next)
        do {
            if (positions.any { it in walls }) return
            val nextBoxes = positions.mapNotNull { boxes.get(it) }.toSet()

            // move everything
            if (nextBoxes.isEmpty()) break

            toMove.addAll(nextBoxes)
            positions = nextBoxes.toCoordinates().map { it.move(direction) }.toSet()
        } while (true)

        for (box in toMove) boxes.remove(box)
        for (box in toMove) boxes.add(box.move(direction))

        robot = next
    }

    fun move(direction: Direction) {
        if (direction.vertical()) // swapped because yeah
            moveHorizontal(direction)
        else moveVertical(direction)
    }

    fun toMap(): Map<Coordinate, Char> {
        val map = mutableMapOf<Coordinate, Char>()
        map[robot] = '@'
        map.putAll(boxes.map { it.first }.associateWith { '[' })
        map.putAll(boxes.map { it.second }.associateWith { ']' })
        map.putAll(walls.associateWith { '#' })
        return map
    }

    fun gps(): Long = boxes.sumOf { it.gps() }
}

fun parseLargeMap(input: List<String>): LargeState {
    var robot: Coordinate? = null
    val boxes = mutableSetOf<Box>()
    val walls = mutableSetOf<Coordinate>()

    for (row in input.indices) {
        for (col in input[row].indices) {
            val coordinate = Coordinate(row, col * 2)
            val pair = Coordinate(coordinate.x, coordinate.y + 1)
            when (input[row][col]) {
                '#' -> {
                    walls.add(coordinate)
                    walls.add(pair)
                }
                'O' -> boxes.add(Pair(coordinate, pair))
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
    val data = readData(Year._2024, 15)
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
