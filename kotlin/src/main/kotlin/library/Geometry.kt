package library

import kotlin.math.abs

enum class Incline {
    ZERO, POSITIVE, NEGATIVE, INFINITE, NONE
}

data class Position(val x: Int, val y: Int) {

    fun incline(other: Position): Incline =
        if (this == other) Incline.NONE
        else if (this.x == other.x) Incline.INFINITE
        else if (this.y == other.y) Incline.ZERO
        else if (this.x < other.x) (
            if (this.y < other.y) Incline.POSITIVE
            else Incline.NEGATIVE
        ) else if (this.y < other.y) Incline.NEGATIVE else Incline.NONE

}

data class Line(private val start: Position, private val end: Position) {
    private val length = abs(end.x - start.x)
    private val height = abs(end.y - start.y)

    val straight = length == 0 || height == 0

    private val deltaX = end.x.compareTo(start.x)
    private val deltaY = end.y.compareTo(start.y)

    fun generatePositions(): Set<Position> {
        val result = mutableSetOf<Position>()

        val points = maxOf(length, height)
        var x = start.x
        var y = start.y
        for (index in 0 .. points) {
            result.add(Position(x, y))
            x += deltaX
            y += deltaY
        }

        return result
    }

}
