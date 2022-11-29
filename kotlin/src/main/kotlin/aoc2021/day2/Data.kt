package aoc2021.day2

abstract class Position {
    abstract val horizontal: Int
    abstract val depth: Int
}

data class SimplePosition(
    override val horizontal: Int = 0,
    override val depth: Int = 0
) : Position()

data class ComplexPosition(
    override val horizontal: Int = 0,
    override val depth: Int = 0,
    val aim: Int = 0
) : Position()

fun Position.summary() = horizontal * depth

val further: (SimplePosition, Int) -> SimplePosition
    get() = { pos: SimplePosition, steps: Int ->
        SimplePosition(pos.horizontal + steps, pos.depth)
    }

val deeper: (SimplePosition, Int) -> SimplePosition
    get() = { pos: SimplePosition, steps: Int ->
        SimplePosition(pos.horizontal, pos.depth + steps)
    }

val followAim: (ComplexPosition, Int) -> ComplexPosition
    get() = { pos: ComplexPosition, steps: Int ->
        ComplexPosition(pos.horizontal + steps, pos.depth + (pos.aim * steps), pos.aim)
    }

val adjustAim: (ComplexPosition, Int) -> ComplexPosition
    get() = { pos: ComplexPosition, steps: Int ->
        ComplexPosition(pos.horizontal, pos.depth, pos.aim + steps)
    }

enum class Command(
    val simpleExecutor: (SimplePosition, Int) -> SimplePosition,
    val complexExecutor: (ComplexPosition, Int) -> ComplexPosition
) {
    FORWARD(further, followAim),
    DOWN(deeper, adjustAim),
    UP(
        { pos: SimplePosition, i: Int -> deeper(pos, -i) },
        { pos: ComplexPosition, i: Int -> adjustAim(pos, -i) }
    );
}

data class Instruction(
    val command: Command,
    val steps: Int
)
