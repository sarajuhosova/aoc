package aoc2022.day17

import library.Year
import library.parseByGroup
import library.readData
import library.readFirst

const val CAVERN_SIZE = 7
const val OFFSET = 2
const val HEIGHT = 3

data class Location(
    val x: Int,
    val y: Int
)

data class Piece(
    val grid : List<List<Boolean>>
) {
    val lowest = (grid[0].indices).map { j ->
        grid.indices.filter { i -> grid[i][j] }.min()
    }

    val highest = (grid[0].indices).map { j ->
        grid.indices.filter { i -> grid[i][j] }.max()
    }
}

fun parsePieces(data: List<String>): List<Piece> =
    parseByGroup(data, "") { lines ->
        Piece(lines.map { it.map { c -> c == '#' } }.reversed())
    }

fun parseJets(line: String): List<Boolean> =
    line.map { it == '<' }

fun jetMove(piece: Piece, dir: Boolean, loc: Location, highest: Array<Int>): Location {
    if (dir && loc.x > 0 && highest[loc.x - 1] < loc.y + piece.lowest[0])
        return Location(loc.x - 1, loc.y)
    else if ((loc.x + piece.grid[0].size) < highest.size && highest[loc.x + 1] < loc.y + piece.lowest[piece.lowest.size - 1])
        return Location(loc.x + 1, loc.y)

    return loc
}

fun downMove(piece: Piece, loc: Location, highest: Array<Int>): Location {
    for (i in piece.lowest.indices) {
        if (piece.lowest[i] + loc.y <= highest[loc.x] + 1) return loc
    }
    return Location(loc.x, loc.y - 1)
}

fun dropPiece(
    piece: Piece,
    jets: List<Boolean>,
    jetsIndex: Int,
    highest: Array<Int>
): Pair<Location, Int> {
    var loc = Location(OFFSET, highest.max() + HEIGHT + 1)

    var index = jetsIndex
    while (true) {
        val shift = jetMove(piece, jets[index], loc, highest)
        loc = downMove(piece, shift, highest)

        index = (index + 1) % jets.size

        if (shift == loc) break
    }
    return Pair(loc, index)
}

fun updateHighest(piece: Piece, location: Location, highest: Array<Int>) {
    val bottom = location.y
    for (i in piece.highest.indices) {
        highest[location.x + i] = bottom + piece.highest[i]
    }
}

fun getTowerHeight(pieces: List<Piece>, jets: List<Boolean>, amount: Int): Int {
    var jetsIndex = 0
    val highest = Array(CAVERN_SIZE) { _ -> -1 }

    for (pieceIndex in (0 until amount)) {
        val piece = pieces[pieceIndex % pieces.size]

        val stop = dropPiece(piece, jets, jetsIndex, highest)
        jetsIndex = stop.second
        updateHighest(piece, stop.first, highest)
    }

    return highest.max()
}

fun main() {
    val pieces = parsePieces(readData(Year._2022, "day17_pieces.txt"))
    val jets = parseJets(readFirst(Year._2022, "day17_e.txt"))

    println(getTowerHeight(pieces, jets, 2022))

}
