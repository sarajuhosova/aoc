package aoc2019.intcode

data class State(private val memory: Memory) {
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

    // MEMORY
    fun read(offset: Int = 0): Int {
        check()
        return memory[ip + offset]
    }
    fun readLoc(offset: Int = 0): Int {
        check()
        return memory[memory[ip + offset]]
    }
    fun updateLoc(value: Int, offset: Int = 0) {
        check()
        memory[memory[ip + offset]] = value
    }
}
