package aoc2019.intcode.io.output

class Output(private var out: Long? = null) {
    fun get(): Long? = out
    fun set(v: Long) { out = v }
}
