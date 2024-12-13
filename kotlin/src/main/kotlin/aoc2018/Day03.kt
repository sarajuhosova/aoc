package aoc2018

import library.*
import library.geometry.ints.Coordinate
import library.geometry.ints.Square

data class Claim(val id: Int, val dimensions: Square) {
    val positions = dimensions.generatePositions()

    var overlaps = false

    override fun toString() =
        "#$id @ ${dimensions.corner.x},${dimensions.corner.y}: ${dimensions.size.x}x${dimensions.size.y}"
}

fun getOverlaps(claims: List<Claim>): Int {
    val claimed = mutableMapOf<Coordinate, Claim>()
    val overlap = mutableSetOf<Coordinate>()

    for (claim in claims) {
        val positions = claim.positions
        for (position in positions) {
            if (position in claimed) {
                overlap.add(position)
                claim.overlaps = true
                // the first one to add this position also needs to be marked as overlapped
                claimed[position]!!.overlaps = true
            } else claimed[position] = claim
        }
    }

    return overlap.size
}

fun main() {
    val claims = readData(Year._2018, 3)
        .parseRegex("#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)") {
            Claim(
                it[0].toInt(),
                Square(
                    Coordinate(it[1].toInt(), it[2].toInt()),
                    Coordinate(it[3].toInt(), it[4].toInt())
                )
            )
        }

    println(getOverlaps(claims))
    println(claims.find { !it.overlaps }!!.id)
}
