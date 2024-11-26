package aoc2019.intcode.io.output

class OutputList() {
    private val list = mutableListOf<Long>()

    fun add(v: Long) { list.add(v) }
    fun get(): List<Long> = list

    operator fun get(index: Int): Long = list[index]
    operator fun set(index: Int, v: Long) { list[index] = v }
}
