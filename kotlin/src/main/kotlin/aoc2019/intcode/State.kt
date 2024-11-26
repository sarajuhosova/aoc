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

            keepAlive(instruction, params, data)
        }
    }
}

fun keepAlive(vararg bla: Any) {

}
