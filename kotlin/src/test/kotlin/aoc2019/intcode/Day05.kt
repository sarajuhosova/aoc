package aoc2019.intcode

import aoc2019.intcode.io.DefaultIO
import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import kotlin.test.Test

class Day05: ComputerTest(5) {

    @Test
    fun examplePart1Test() {
        val computer = Computer("1002,4,3,4,33")

        computer.run()
        assertThat(computer.readResult(4)).isEqualTo(99)
    }

    @Test
    fun part1Test() {
        val results = mutableListOf<Int>()

        class Part1IO() : DefaultIO() {
            override fun read(): Int = 1

            override fun write(out: Int) {
                results.add(out)
            }
        }

        computer().run(io = Part1IO())
        assertThat(results.subList(0, results.size - 1)).allMatch { it == 0 }
        assertThat(results.last()).isEqualTo(9961446)
    }

    private val COMPARISON_EXAMPLES = listOf(
        Triple("3,9,8,9,10,9,4,9,99,-1,8", 8, -2352),
        Triple("3,9,7,9,10,9,4,9,99,-1,8", -42357982, 8),
        Triple("3,3,1108,-1,8,3,4,3,99", 8, 975),
        Triple("3,3,1107,-1,8,3,4,3,99", 0, 8)
    )

    @TestFactory
    fun examplePart2ComparisonTests() = COMPARISON_EXAMPLES.map { (mem, t, f) ->
        DynamicTest.dynamicTest("Input $mem results in true for input $t and false for input $f") {
            val computer = Computer(mem)

            class Part2IO(val input: Int, val expected: Boolean) : DefaultIO() {
                override fun read(): Int = input

                override fun write(out: Int) {
                    if (expected) assertThat(out).isEqualTo(1)
                    else assertThat(out).isEqualTo(0)
                }
            }

            computer.run(io = Part2IO(t, true))
            computer.run(io = Part2IO(f, false))

            assertThat(computer.readResult()).isNotNull()
        }
    }

    private val JUMP_EXAMPLES = listOf(
        "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9",
        "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
    )

    @TestFactory
    fun examplePart2JumpTests() = JUMP_EXAMPLES.map { mem ->
        DynamicTest.dynamicTest("Input $mem results returns zero is input is zero, else 1)") {
            val computer = Computer(mem)

            class Part2IO(val input: Int) : DefaultIO() {
                override fun read(): Int = input

                override fun write(out: Int) {
                    if (input == 0) assertThat(out).isEqualTo(0)
                    else assertThat(out).isEqualTo(1)
                }
            }

            computer.run(io = Part2IO(0))
            computer.run(io = Part2IO(-2495761))
            computer.run(io = Part2IO(97645))

            assertThat(computer.readResult()).isNotNull()
        }
    }

    @Test
    fun examplePart2Test() {
        val computer = Computer(
            "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
                    + "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
                    + "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        )

        class Part2IO(val input: Int) : DefaultIO() {
            override fun read(): Int = input

            override fun write(out: Int) {
                if (input < 8) assertThat(out).isEqualTo(999)
                else if (input == 8) assertThat(out).isEqualTo(1000)
                else assertThat(out).isEqualTo(1001)
            }
        }

        computer.run(io = Part2IO(8))
        computer.run(io = Part2IO(7))
        computer.run(io = Part2IO(9))

        assertThat(computer.readResult()).isNotNull()
    }

    @Test
    fun part2Test() {
        class Part2IO() : DefaultIO() {
            override fun read(): Int = 5

            override fun write(out: Int) {
                assertThat(out).isEqualTo(742621)
            }
        }

        computer().run(io = Part2IO())

        assertThat(computer().readResult()).isNotNull()
    }

}