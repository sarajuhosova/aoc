package library.geometry.ints

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
