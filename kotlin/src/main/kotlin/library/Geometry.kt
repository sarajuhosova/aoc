package library

import kotlin.math.abs

enum class Direction {
    UP, RIGHT, DOWN, LEFT;

    fun clockwise(): Direction = entries[(this.ordinal + 1) % entries.size]
    fun counterClockwise(): Direction = entries[(this.ordinal - 1 + entries.size) % entries.size]
}

enum class Incline {
    ZERO, POSITIVE, NEGATIVE, INFINITE, NONE
}

data class Coordinate(val x: Int, val y: Int) {
    fun inBounds(bounds: Coordinate): Boolean =
        0 <= this.x && this.x < bounds.x && 0 <= this.y && this.y < bounds.y

    fun getNeighbours(): Set<Coordinate> = setOf(
        this.move(Direction.UP),
        this.move(Direction.RIGHT),
        this.move(Direction.DOWN),
        this.move(Direction.LEFT)
    )

    fun getDiagonals(): Set<Coordinate> = setOf(
        this.move(Direction.UP, Direction.RIGHT),
        this.move(Direction.UP, Direction.LEFT),
        this.move(Direction.DOWN, Direction.RIGHT),
        this.move(Direction.DOWN, Direction.LEFT),
    )

    fun getSurrounding(): Set<Coordinate> =
        getNeighbours() union getDiagonals()

    fun move(direction: Direction): Coordinate = when (direction) {
        Direction.UP -> Coordinate(x, y + 1)
        Direction.RIGHT -> Coordinate(x + 1, y)
        Direction.DOWN -> Coordinate(x, y - 1)
        Direction.LEFT -> Coordinate(x - 1, y)
    }

    fun move(vararg directions: Direction): Coordinate {
        var result = this
        for (direction in directions) {
            result = result.move(direction)
        }
        return result
    }

    fun moveSteps(steps: Int, vararg directions: Direction): List<Coordinate> {
        val result = mutableListOf<Coordinate>()
        var current = this
        repeat (steps) {
            current = current.move(*directions)
            result.add(current)
        }
        return result
    }

    fun manhattanDistance(other: Coordinate): Int = abs(x - other.x) + abs(y - other.y)

    fun incline(other: Coordinate): Incline =
        if (this == other) Incline.NONE
        else if (this.x == other.x) Incline.INFINITE
        else if (this.y == other.y) Incline.ZERO
        else if (this.x < other.x) (
            if (this.y < other.y) Incline.POSITIVE
            else Incline.NEGATIVE
        ) else if (this.y < other.y) Incline.NEGATIVE else Incline.NONE

}

fun Map<Coordinate, Char>.draw(default: Char = ' '): String {
    val smallestX = this.keys.minOf { it.x }
    val largestX = this.keys.maxOf { it.x }
    val smallestY = this.keys.minOf { it.y }
    val largestY = this.keys.maxOf { it.y }

    val result = mutableListOf<String>()
    for (x in smallestX .. largestX) {
        var string = ""
        for (y in smallestY .. largestY) {
            string += this.getOrDefault(Coordinate(x, y), default)
        }
        result.add(string)
    }
    return result.joinToString("\n")
}

data class Line(private val start: Coordinate, private val end: Coordinate) {
    private val length = abs(end.x - start.x)
    private val height = abs(end.y - start.y)

    val straight = length == 0 || height == 0

    private val deltaX = end.x.compareTo(start.x)
    private val deltaY = end.y.compareTo(start.y)

    fun generatePositions(): Set<Coordinate> {
        val result = mutableSetOf<Coordinate>()

        val points = maxOf(length, height)
        var x = start.x
        var y = start.y
        for (index in 0 .. points) {
            result.add(Coordinate(x, y))
            x += deltaX
            y += deltaY
        }

        return result
    }
}

data class Square(
    val corner: Coordinate,
    val size: Coordinate
) {
    /**
     * The opposite corner.
     */
    private val opposite = Coordinate(corner.x + size.x - 1, corner.y + size.y - 1)

    fun generatePositions(): Set<Coordinate> = (corner.x .. opposite.x).flatMap { x ->
        (corner.y .. opposite.y).map { y -> Coordinate(x, y) }
    }.toSet()

}
