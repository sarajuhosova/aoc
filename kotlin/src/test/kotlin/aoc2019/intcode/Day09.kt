package aoc2019.intcode

import aoc2019.intcode.io.output.MultiOutputIO
import aoc2019.intcode.io.output.Output
import aoc2019.intcode.io.output.OutputList
import aoc2019.intcode.io.output.SingleOutputIO
import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions.assertThat
import kotlin.test.Test

class Day09: ComputerTest(9) {

    @Test
    fun example1Part1Test() {
        val computer = Computer("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")

        val output = OutputList()

        runBlocking { computer.run(io = MultiOutputIO(output)) }

        assertThat(output.get()).containsExactly(
            109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99
        )
    }

    @Test
    fun example2Part1Test() {
        val computer = Computer("1102,34915192,34915192,7,4,7,99,0")

        val output = Output()

        runBlocking { computer.run(io = SingleOutputIO(output)) }

        // has 16 digits
        assertThat(output.get().toString().length).isEqualTo(16)
    }

    @Test
    fun example3Part1Test() {
        val computer = Computer("104,1125899906842624,99")

        val output = Output()

        runBlocking { computer.run(io = SingleOutputIO(output)) }

        assertThat(output.get()).isEqualTo(1125899906842624)
    }

    @Test
    fun part1Test() {
        val output = OutputList()

        class Day09IO: MultiOutputIO(output) {
            override suspend fun provide(): Long = 1
        }

        runBlocking { computer().run(io = Day09IO()) }

        assertThat(output.get()).containsExactly(4288078517)
    }

    @Test
    fun part2Test() {
        val output = OutputList()

        class Day09IO: MultiOutputIO(output) {
            override suspend fun provide(): Long = 2
        }

        runBlocking { computer().run(io = Day09IO()) }

        assertThat(output.get()).containsExactly(69256)
    }

}