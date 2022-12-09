package aoc2022.day09

import library.Year
import library.head
import library.readData
import kotlin.math.abs

enum class Direction() {
    D, U, L, R
}

data class Position(
    val x: Int = 0,
    val y: Int = 0
)

fun parse(lines: List<String>): List<Direction> =
    lines.map { it.split(" ") }
        .flatMap {
            (0 until it[1].toInt())
                .map { _-> Direction.valueOf(it[0]) }
        }

fun touching(head: Position, tail: Position): Boolean =
    abs(head.x - tail.x) <= 1 && abs(head.y - tail.y) <= 1

fun moveHead(head: Position, direction: Direction): Position =
    when (direction) {
        Direction.D -> Position(head.x, head.y - 1)
        Direction.U -> Position(head.x, head.y + 1)
        Direction.L -> Position(head.x - 1, head.y)
        Direction.R -> Position(head.x + 1, head.y)
    }

fun moveKnot(current: Position, next: Position): Position {
    if (touching(current, next)) return next

    val cmpY = current.y.compareTo(next.y)

    return when (current.x.compareTo(next.x)) {
        -1 -> Position(
            next.x - 1,
            when (cmpY) {
                -1 -> next.y - 1
                1 -> next.y + 1
                else -> next.y
            }
        )
        1 -> Position(
            next.x + 1,
            when (cmpY) {
                -1 -> next.y - 1
                1 -> next.y + 1
                else -> next.y
            }
        )
        else -> Position(
            next.x,
            when (cmpY) {
                -1 -> next.y - 1
                1 -> next.y + 1
                else -> next.y // should never be reached
            }
        )
    }
}

fun moveRope(head: Position, rest: List<Position>): List<Position> {
    val result = mutableListOf<Position>()
    result.add(head)
    for (next in rest) {
        result.add(moveKnot(result.last(), next))
    }
    return result
}

fun move(current: List<Position>, direction: Direction): List<Position> =
    moveRope(moveHead(current.head(), direction), current.drop(1))

fun simulate(directions: List<Direction>, length: Int): Int {
    val visited = mutableSetOf<Position>()

    var current = List(length) { _ -> Position() }
    visited.add(current.last())

    for (direction in directions) {
        current = move(current, direction)
        visited.add(current.last())
    }

    return visited.size
}

fun main() {
    val moves = parse(readData(Year._2022, "day09.txt"))

    println(simulate(moves, 2))
    println(simulate(moves, 10))
}