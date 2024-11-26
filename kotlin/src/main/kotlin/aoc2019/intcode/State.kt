package aoc2019.intcode

import aoc2019.intcode.expections.ProgramHaltedException
import aoc2019.intcode.instructions.Instruction
import aoc2019.intcode.io.IO
import aoc2019.intcode.parameters.Param
import aoc2019.intcode.parameters.ParamMode
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
    fun getIP() = ip
    fun movePointer(move: Int) {
        ip += move
    }
    fun setPointer(location: Int) {
        ip = location
    }

    private var base = 0
    fun getBase() = base
    fun moveBase(offset: Int) {
        base += offset
    }

    private fun getLocation(param: Param): Int =
        param.mode.getLocation(this, param.offset)

    // MEMORY
    fun read(offset: Int = 0): Int {
        check()
        return memory[ip + offset]
    }
    fun read(param: Param): Int {
        check()
        return memory[getLocation(param)]
    }
    fun update(value: Int, param: Param) {
        check()
        memory[getLocation(param)] = value
    }

    // IO
    suspend fun input(): Int = io.read()
    suspend fun output(value: Int) = io.write(value)

    // EXECUTION
    suspend fun execute() {
        while (true) {
            val data = this.read()

            // prepare the instructions and parameters
            val instruction = Instruction.get(data % 100)
            val params = Array(instruction.arguments) { i ->
                val position = pow(10, i + 2)
                Param(ParamMode[(data / position) % 10], i + 1)
            }

            // update the state by executing the instruction
            instruction.execute(this, params)

            // check whether the execution was halted
            if (this.isHalted()) return
        }
    }
}
