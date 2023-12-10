package aoc2023.day10

import library.Year
import library.readData

typealias Position = Pair<Int, Int>
typealias PipeMap = Map<Position, Pipe>

enum class Direction(
    val next: (pos: Position) -> Position
) {
    UP({ pos -> Pair(pos.first - 1, pos.second) }),
    RIGHT({ pos -> Pair(pos.first, pos.second + 1) }),
    DOWN({ pos -> Pair(pos.first + 1, pos.second) }),
    LEFT({ pos -> Pair(pos.first, pos.second - 1) })
}

val OPPOSITE = mapOf(
    Pair(Direction.UP, Direction.DOWN),
    Pair(Direction.DOWN, Direction.UP),
    Pair(Direction.RIGHT, Direction.LEFT),
    Pair(Direction.LEFT, Direction.RIGHT)
)

enum class Pipe(
    val char: Char,
    val directions: Pair<Direction, Direction>
) {
    NS(
        '|',
        Pair(Direction.UP, Direction.DOWN)
    ),
    EW(
        '-',
        Pair(Direction.LEFT, Direction.RIGHT)
    ),
    NE(
        'L',
        Pair(Direction.UP, Direction.RIGHT)
    ),
    NW(
        'J',
        Pair(Direction.UP, Direction.LEFT)
    ),
    SW(
        '7',
        Pair(Direction.DOWN, Direction.LEFT)
    ),
    SE(
        'F',
        Pair(Direction.DOWN, Direction.RIGHT)
    );
}

val FROM_CHAR: Map<Char, Pipe> = Pipe.values().associateBy { it.char }

val VALID = mapOf(
    Pair(Direction.UP, listOf(Pipe.NS, Pipe.SE, Pipe.SW)),
    Pair(Direction.RIGHT, listOf(Pipe.EW, Pipe.NW, Pipe.SW)),
    Pair(Direction.DOWN, listOf(Pipe.NS, Pipe.NE, Pipe.NW)),
    Pair(Direction.LEFT, listOf(Pipe.EW, Pipe.NE, Pipe.SE))
)

fun identifyStart(start: Position, pipes: PipeMap): Pipe {
    val dirs = mutableSetOf<Direction>()

    for (dir in Direction.values()) {
        val pos = dir.next(start)
        val pipe = pipes[pos]
        if (pipe != null && pipe in VALID[dir]!!) dirs.add(dir)
    }

    return Pipe.values().first {
        dirs.contains(it.directions.first) && dirs.contains(it.directions.second)
    }
}

fun parse(data: List<String>): Pair<Position, PipeMap> {
    val pipes = mutableMapOf<Position, Pipe>()
    var start: Position = Pair(-1, -1)
    for (i in data.indices) {
        for (j in data[i].indices) {
            val char = data[i][j]
            val maybe = FROM_CHAR[char]
            if (maybe != null) pipes[Pair(i, j)] = maybe
            if (char == 'S') start = Pair(i, j)
        }
    }
    pipes[start] = identifyStart(start, pipes)

    return Pair(start, pipes)
}

fun pickFirst(start: Position, pipes: PipeMap): Triple<Position, Pipe, Direction>? {
    for (dir in Direction.values()) {
        val pos = dir.next(start)
        val pipe = pipes[pos]
        if (pipe != null && pipe in VALID[dir]!!) return Triple(pos, pipe, dir)
    }

    return null
}

fun getNext(current: Position, pipe: Pipe, from: Direction): Pair<Position, Direction> {
    val to = if (pipe.directions.first == from) pipe.directions.second else pipe.directions.first
    return Pair(to.next(current), to)
}

fun findLoop(start: Position, pipes: PipeMap): List<Position> {
    var (pos, pipe, to) = pickFirst(start, pipes)!!

    val path = mutableListOf(start)

    while (true) {
        path.add(pos)
        val (nextPos, nextTo) = getNext(pos, pipe, OPPOSITE[to]!!)

        if (nextPos == start) break

        pos = nextPos
        pipe = pipes[pos]!!
        to = nextTo
    }

    return path
}

fun isInside(
    space: Position, inside: Map<Position, Boolean>,
    minI: Int, loop: List<Position>, pipes: PipeMap
): Boolean {
    var pos = Pair(space.first - 1, space.second)
    var crosses = 0

    var latest: Pipe? = null

    while (pos.first >= minI) {
        if (pos in loop) {
            when (val pipe = pipes[pos]!!) {
                Pipe.EW -> crosses++
                Pipe.NW, Pipe.NE -> latest = pipe
                Pipe.SW -> {
                    if (latest == Pipe.NE) crosses++
                    latest = null
                }
                Pipe.SE -> {
                    if (latest == Pipe.NW) crosses++
                    latest = null
                }
                else -> {}
            }
        } else if (pos in inside) {
            return (inside[pos]!!).xor(crosses % 2 == 1)
        }

        pos = Pair(pos.first - 1, pos.second)
    }

    return crosses % 2 == 1
}

fun countInnerTiles(loop : List<Position>, pipes: PipeMap): Int {
    val minI = loop.minOf { it.first }
    val inside = mutableMapOf<Position, Boolean>()

    for (i in minI..loop.maxOf { it.first }) {
        for (j in loop.minOf { it.second }..loop.maxOf { it.second }) {
            val pos = Pair(i, j)
            if (pos !in loop) {
                inside[pos] = isInside(pos, inside, minI, loop, pipes)
            }
        }
    }

    return inside.values.count { it }
}

fun main() {
    val (start, pipes) = parse(readData(Year._2023, 10))

    val loop = findLoop(start, pipes)

    println(loop.size / 2L)
    println(countInnerTiles(loop, pipes))
}
