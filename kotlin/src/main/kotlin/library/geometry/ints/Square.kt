package library.geometry.ints

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
