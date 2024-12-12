package library.geometry

import kotlin.math.abs

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
