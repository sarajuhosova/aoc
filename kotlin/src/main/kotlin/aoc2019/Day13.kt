package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.IO
import kotlinx.coroutines.runBlocking
import library.Coordinate
import library.Year
import library.readFirst

enum class Tile {
    EMPTY,  // 0 is an empty tile. No game object appears in this tile.
    WALL,   // 1 is a wall tile. Walls are indestructible barriers.
    BLOCK,  // 2 is a block tile. Blocks can be broken by the ball.
    PADDLE, // 3 is a horizontal paddle tile. The paddle is indestructible.
    BALL;    // 4 is a ball tile. The ball moves diagonally and bounces off objects.

    companion object {
        private val map = entries.associateBy { it.ordinal }
        fun getTile(id: Int) : Tile = map[id] ?: EMPTY
    }
}

class ArcadeGame : IO() {
    private val tiles = mutableMapOf<Coordinate, Tile>()
    fun getTiles(): Map<Coordinate, Tile> = tiles

    enum class Input {
        X, Y, ID;

        fun next() = entries[(this.ordinal + 1) % entries.size]
    }
    private var expected = Input.X
    private var x: Int? = null
    private var y: Int? = null

    override suspend fun provide(): Long {
        TODO("Not yet implemented")
    }

    override suspend fun write(out: Long) {
        when (expected) {
            Input.X -> x = out.toInt()
            Input.Y -> y = out.toInt()
            Input.ID -> tiles[Coordinate(x!!, y!!)] = Tile.getTile(out.toInt())
        }
        expected = expected.next()
    }
}

fun main() {
    val computer = Computer(readFirst(Year._2019, 13))

    val game1 = ArcadeGame()
    runBlocking { computer.run(io = game1) }
    println(game1.getTiles().values.count { it == Tile.BLOCK })
}
