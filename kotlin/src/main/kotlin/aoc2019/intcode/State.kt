package aoc2019.intcode

import aoc2019.intcode.expections.ProgramHaltedException
import aoc2019.intcode.instructions.Param
import aoc2019.intcode.io.IO

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
}
