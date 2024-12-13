package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.IO
import kotlinx.coroutines.runBlocking
import library.*
import library.geometry.ints.Coordinate
import library.geometry.Direction
import library.geometry.ints.draw

// 0 -> black | left
// 1 -> white | right

class Robot(initial: Char = ' '): IO() {
    private var direction: Direction = Direction.UP
    private var position = Coordinate(0, 0)
    private val painted = mutableMapOf(position to initial)

    fun getPainted(): Map<Coordinate, Char> = painted

    override suspend fun provide(): Long =
        if (position in painted && painted[position]!! != ' ') 1 else 0

    private var painting = true

    override suspend fun write(out: Long) {
        if (painting) {
            painted[position] = if (out != 0L) '#' else ' '
        } else {
            direction = if (out == 0L) direction.counterClockwise() else direction.clockwise()
            position = position.move(direction)
        }
        painting = !painting
    }
}

fun main() {
    val computer = Computer(readFirst(Year._2019, 11))

    val robot1 = Robot()
    runBlocking { computer.run(io = robot1) }
    println(robot1.getPainted().size)

    val robot2 = Robot('#')
    runBlocking { computer.run(io = robot2) }
    println(robot2.getPainted().draw())
}
