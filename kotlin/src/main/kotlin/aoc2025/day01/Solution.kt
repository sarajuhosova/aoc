package aoc2025.day01

import library.Year
import library.readData

const val START_POSITION = 50
const val DIAL_SIZE = 100

typealias Operation = (Int, Int) -> Int
typealias Rotation = Pair<Operation, Int>

fun modulate(position: Int): Int = ((position % DIAL_SIZE) + DIAL_SIZE) % DIAL_SIZE

fun parse(command: String): Rotation {
    val moves = command.substring(1).toInt()
    val operation: Operation = when (command[0]) {
        'L' -> { i, j -> i - j }
        'R' -> { i, j -> i + j }
        else -> throw IllegalArgumentException("Unknown command: $command")
    }
    return Pair(operation, moves)
}

fun part1(rotations: List<Rotation>): Int {
    var count = 0
    var position = START_POSITION
    for (rotation in rotations) {
        position = modulate(rotation.first(position, rotation.second))
        if (position == 0) count++
    }

    return count
}

fun part2(rotations: List<Rotation>): Int {
    var count = 0
    var position = START_POSITION

    for (rotation in rotations) {
        var moves = rotation.second
        count += moves / DIAL_SIZE
        moves %= DIAL_SIZE

        position = rotation.first(position, moves)
        if (position == 0) count++
        else if (position < 0 || position > DIAL_SIZE) {
            count++
            position = modulate(position)
        }
    }

    return count
}

fun main() {
    val rotations = readData(Year._2025, 1).map(::parse)

    println(part1(rotations))
    println(part2(rotations))
}
