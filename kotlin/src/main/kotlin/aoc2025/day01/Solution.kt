package aoc2025.day01

import library.Year
import library.readData

const val DIAL_SIZE = 100

fun parse(command: String): (Int) -> Int {
    val moves = command.substring(1).toInt()
    return when (command[0]) {
        'R' -> { i -> (i + moves) % DIAL_SIZE }
        'L' -> { i -> ((i - moves) % DIAL_SIZE + DIAL_SIZE) % DIAL_SIZE }
        else -> throw IllegalArgumentException("Unknown command: $command")
    }
}

fun part1(commands: List<String>): Int {
    val rotations = commands.map { parse(it) }

    var count = 0
    var position = 50
    for (f in rotations) {
        position = f(position)
        if (position == 0) count++
    }

    return count
}

fun part2(commands: List<String>): Int {
    var count = 0
    var position = 50

    for (c in commands) {

        var moves = c.substring(1).toInt()
        count += moves / DIAL_SIZE
        moves %= DIAL_SIZE

        when (c[0]) {
            'R' -> {
                position += moves
                if (position > DIAL_SIZE) {
                    count++
                    position -= DIAL_SIZE
                }
            }
            'L' -> {
                position -= moves
                if (position < 0) {
                    count++
                    position += DIAL_SIZE
                }
            }
        }

        if (position == 0) count++
    }

    return count
}

fun main() {
    val commands = readData(Year._2025, 1)

    println(part1(commands))
    println(part2(commands))
}
