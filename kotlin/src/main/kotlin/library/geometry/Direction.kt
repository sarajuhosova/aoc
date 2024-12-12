package library.geometry

enum class Direction {
    UP, RIGHT, DOWN, LEFT;

    fun clockwise(): Direction = entries[(this.ordinal + 1) % entries.size]
    fun counterClockwise(): Direction = entries[(this.ordinal - 1 + entries.size) % entries.size]
}
