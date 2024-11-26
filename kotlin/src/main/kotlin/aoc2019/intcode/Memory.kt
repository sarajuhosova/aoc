package aoc2019.intcode

class Memory() {
    private val map: MutableMap<Int, Int> = mutableMapOf()

    constructor(input: String) : this() {
        map.putAll(input.split(",")
                .mapIndexed { index, s -> index to s.toInt() })
    }

    constructor(data: Map<Int, Int>) : this() {
        map.putAll(data)
    }

    fun copy() = Memory(map)

    operator fun get(index: Int): Int = map.getOrDefault(index, 0)
    operator fun set(index: Int, value: Int) {
        map[index] = value
    }
}
