package aoc2019.intcode

class Memory() {
    private val map: MutableMap<Int, Long> = mutableMapOf()

    constructor(input: String) : this() {
        map.putAll(input.split(",")
                .mapIndexed { index, s -> index to s.toLong() })
    }

    constructor(data: Map<Int, Long>) : this() {
        map.putAll(data)
    }

    fun copy() = Memory(map)

    operator fun get(index: Int): Long = map.getOrDefault(index, 0L)
    operator fun set(index: Int, value: Long) {
        map[index] = value
    }
}
