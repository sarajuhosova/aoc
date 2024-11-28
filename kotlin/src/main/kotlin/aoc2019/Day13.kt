package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.IO
import kotlinx.coroutines.runBlocking
import library.Coordinate
import library.Year
import library.readFirst
import kotlin.random.Random

enum class Tile(val char: Char) {
    EMPTY(' '),  // 0 is an empty tile. No game object appears in this tile.
    WALL('|'),   // 1 is a wall tile. Walls are indestructible barriers.
    BLOCK('□'),  // 2 is a block tile. Blocks can be broken by the ball.
    PADDLE('_'), // 3 is a horizontal paddle tile. The paddle is indestructible.
    BALL('•');    // 4 is a ball tile. The ball moves diagonally and bounces off objects.

    companion object {
        private val map = entries.associateBy { it.ordinal }
        fun getTile(id: Int) : Tile = map[id] ?: EMPTY
    }
}

class ArcadeGame {
    private val blocks = mutableSetOf<Coordinate>()
    private val walls = mutableSetOf<Coordinate>()
    fun blocks(): Set<Coordinate> = blocks

    private var paddle: Coordinate? = null
    private var ball: Coordinate? = null

    private var score = 0L
    fun score() = score
    fun updateScore(s: Long) { score = s }

    fun position(coordinate: Coordinate, tile: Tile) {
        when (tile) {
            Tile.EMPTY -> {} // do nothing
            Tile.WALL -> walls.add(coordinate)
            Tile.BLOCK -> blocks.add(coordinate)
            Tile.PADDLE -> paddle = coordinate
            Tile.BALL -> ball = coordinate
        }
    }
}

class ArcadeGameInterface(private val game: ArcadeGame) : IO() {
    enum class Input {
        X, Y, ID_OR_SCORE;

        fun next() = entries[(this.ordinal + 1) % entries.size]
    }
    private var expected = Input.X
    private var x: Int? = null
    private var y: Int? = null

    override suspend fun provide(): Long = Random.nextLong(-1, 2)

    override suspend fun write(out: Long) {
        when (expected) {
            Input.X -> x = out.toInt()
            Input.Y -> y = out.toInt()
            Input.ID_OR_SCORE -> {
                if (x == -1 && y == 0) {
                    game.updateScore(out)
                } else {
                    game.position(Coordinate(x!!, y!!), Tile.getTile(out.toInt()))
                }
            }
        }
        expected = expected.next()
    }
}

fun main() {
    val computer = Computer(readFirst(Year._2019, 13))

    val game1 = ArcadeGame()
    runBlocking { computer.run(io = ArcadeGameInterface(game1)) }
    println(game1.blocks().size)

    val game2 = ArcadeGame()
    runBlocking { computer.run(mapOf(0 to 2L), io = ArcadeGameInterface(game2)) }
    println(game2.score())
}
