package aoc2019.intcode.io

abstract class IO(private val settings: Array<Int>) {
    private var pointer = -1

    abstract suspend fun provide(): Int

    suspend fun read(): Int {
        pointer++
        if (pointer < settings.size) return settings[pointer]
        return provide()
    }

    abstract suspend fun write(out: Int)
}
