package aoc2019.intcode

import aoc2019.Robot
import kotlinx.coroutines.runBlocking
import library.geometry.ints.draw
import org.assertj.core.api.Assertions.assertThat
import kotlin.test.Test

class Day11: ComputerTest(11) {

    @Test
    fun part1Test() {
        val robot = Robot()
        runBlocking { computer().run(io = robot) }
        assertThat(robot.getPainted().size).isEqualTo(2226)
    }

    private fun String.clean(): String =
        this.dropWhile { it == ' ' || it == '\n' }.dropLastWhile { it == ' ' || it == '\n' }

    @Test
    fun part2Test() {
        val robot = Robot('#')
        runBlocking { computer().run(io = robot) }
        assertThat(robot.getPainted().draw().trimIndent().clean()).isEqualTo("""
            ######
               #  
               #  
            ######
                  
            ######
            #  # #
            #  # #
             ## # 
                  
             #### 
            #    #
            # #  #
            ### # 
                  
            ######
            #     
            #     
            #     
                  
            ##   #
            # #  #
            #  # #
            #   ##
                  
            ######
               #  
             ## # 
            #    #
                  
            ######
            #     
            #     
            #     
                  
            ######
               # #
               # #
                 # 
        """.trimIndent().clean())
    }

}