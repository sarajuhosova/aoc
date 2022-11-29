package aoc2021.day02

import library.Year
import library.readData

fun executeSimple(instructions: List<Instruction>) =
    instructions.fold(SimplePosition()) { pos: SimplePosition, instr: Instruction ->
        instr.command.simpleExecutor(pos, instr.steps)
    }

fun executeComplex(instructions: List<Instruction>) =
    instructions.fold(ComplexPosition()) { pos: ComplexPosition, instr: Instruction ->
        instr.command.complexExecutor(pos, instr.steps)
    }

fun main() {
    val instructions = readData(Year._2021, "day02.txt")
        .map { it.split(" ") }
        .map { Instruction(Command.valueOf(it[0].uppercase()), it[1].toInt()) }

    println(executeSimple(instructions).summary())
    println(executeComplex(instructions).summary())
}