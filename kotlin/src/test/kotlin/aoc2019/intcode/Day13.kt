package aoc2019.intcode

import aoc2019.ArcadeGame
import aoc2019.Tile
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions.assertThat
import kotlin.test.Test

class Day13: ComputerTest(13) {

    @Test
    fun part1Test() {
        val game = ArcadeGame()
        runBlocking { computer().run(io = game) }
        assertThat(game.getTiles().values.count { it == Tile.BLOCK }).isEqualTo(312)
    }

}