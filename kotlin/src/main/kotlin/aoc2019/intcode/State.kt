package aoc2019.intcode

import aoc2019.intcode.expections.ProgramHaltedException
import aoc2019.intcode.instructions.Instruction
import aoc2019.intcode.instructions.Param
import aoc2019.intcode.instructions.ParamMode
import aoc2019.intcode.io.IO
import library.pow

data class State(
    private val memory: Memory,
    private val io: IO
) {
    // RUNNING
    private var running: Boolean = true
    fun isHalted() = !running
    fun halt() { running = false }
    private fun check() {
        if (!running) throw ProgramHaltedException()
    }

    // INSTRUCTION POINTER
    private var ip = 0
    fun movePointer(move: Int) {
        ip += move
    }
    fun setPointer(location: Int) {
        ip = location
    }

    // MEMORY
    fun read(offset: Int = 0): Int {
        check()
        return memory[ip + offset]
    }
    fun read(param: Param): Int {
        check()
        return memory[param.getLocation(memory, ip)]
    }
    fun update(value: Int, param: Param) {
        check()
        memory[param.getLocation(memory, ip)] = value
    }

    // IO
    fun input(): Int = io.read()
    fun output(value: Int) = io.write(value)

    // EXECUTION
    private fun nextInstruction(): Instruction {
        return Instruction.getById(this.read() % 100)
    }

    private fun execute(instruction: Instruction) {
        // resolve the parameters
        val params = Array(instruction.arguments) { i ->
            val position = pow(10, i + 2)
            Param(ParamMode[(this.read() / position) % 10], i + 1)
        }

        // run the instruction
        val jumped = instruction.action(this, params)

        // update the pointer
        if (!jumped) this.movePointer(instruction.arguments + 1)
    }

    fun execute() {
        while (true) {
            this.execute(this.nextInstruction())
            if (this.isHalted()) return
        }
    }
}
