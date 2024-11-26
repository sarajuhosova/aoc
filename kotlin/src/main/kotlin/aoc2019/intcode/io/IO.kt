package aoc2019.intcode.io

abstract class IO(private val settings: Array<Long>) {
    private var pointer = -1

    abstract suspend fun provide(): Long

    suspend fun read(): Long {
        pointer++
        if (pointer < settings.size) return settings[pointer]
        return provide()
    }

    abstract suspend fun write(out: Long)
}
