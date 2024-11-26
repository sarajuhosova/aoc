package aoc2019.intcode

import kotlinx.coroutines.runBlocking
import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.DynamicTest
import org.junit.jupiter.api.TestFactory
import kotlin.test.Test

class Day02: ComputerTest(2) {

    private val EXAMPLES: List<Triple<String, Int, Long>> = listOf(
        Triple("1,9,10,3,2,3,11,0,99,30,40,50", 0, 3500),
        Triple("1,0,0,0,99", 0, 2),
        Triple("2,3,0,3,99", 3, 6),
        Triple("2,4,4,5,99,0", 5, 9801),
        Triple("1,1,1,4,99,5,6,0,99", 0, 30),
        Triple("1,1,1,4,99,5,6,0,99", 4, 2)

    )

    @TestFactory
    fun examplePart1Test() = EXAMPLES.map { (mem, index, result) ->
        DynamicTest.dynamicTest("Input $mem results in $result at position $index") {
            runBlocking {
                val computer = Computer(mem)

                computer.run()
                assertThat(computer.readResult(index)).isEqualTo(result)
            }
        }
    }

    @Test
    fun part1Test() {
        runBlocking {
            computer().run(12, 2)
            assertThat(computer().readResult()).isEqualTo(6568671)
        }
    }

    @Test
    fun part2Test() {
        runBlocking {
            var expected: Long? = null
            for (noun in 0L..99) {
                for (verb in 0L..99) {
                    computer().run(noun, verb)
                    val result = computer().readResult()
                    if (result == 19690720L) {
                        expected = 100 * noun + verb
                        break
                    }
                }
            }

            assertThat(expected).isEqualTo(3951)
        }
    }

}