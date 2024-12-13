package library.geometry.longs

import library.geometry.Direction
import library.geometry.Incline
import kotlin.math.abs

data class Coordinate(val x: Long, val y: Long) {
    fun inBounds(x: Long, y: Long): Boolean =
        this.x in 0..<x && this.y in 0..<y

    fun inBounds(bounds: Coordinate): Boolean = inBounds(bounds.x, bounds.y)

    fun copy(): Coordinate = Coordinate(x, y)

    operator fun plus(other: Coordinate): Coordinate =
        Coordinate(x + other.x, y + other.y)

    operator fun plus(scalar: Long): Coordinate = Coordinate(x + scalar, y + scalar)

    operator fun minus(other: Coordinate): Coordinate =
        Coordinate(x - other.x, y - other.y)

    operator fun times(scalar: Long): Coordinate = Coordinate(x * scalar, y * scalar)

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

    fun manhattanDistance(other: Coordinate): Long = abs(x - other.x) + abs(y - other.y)

    fun incline(other: Coordinate): Incline =
        if (this == other) Incline.NONE
        else if (this.x == other.x) Incline.INFINITE
        else if (this.y == other.y) Incline.ZERO
        else if (this.x < other.x) (
                if (this.y < other.y) Incline.POSITIVE
                else Incline.NEGATIVE
                ) else if (this.y < other.y) Incline.NEGATIVE else Incline.NONE

    companion object {
        fun origin(): Coordinate = Coordinate(0, 0)
    }

}
