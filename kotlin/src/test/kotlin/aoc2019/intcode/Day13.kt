package aoc2019.intcode

import aoc2019.ArcadeGameInterface
import aoc2019.ArcadeGame
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions.assertThat
import kotlin.test.Test

class Day13: ComputerTest(13) {

    @Test
    fun part1Test() {
        val game = ArcadeGame()
        runBlocking { computer().run(io = ArcadeGameInterface(game)) }
        assertThat(game.blocks()).hasSize(312)
    }

}