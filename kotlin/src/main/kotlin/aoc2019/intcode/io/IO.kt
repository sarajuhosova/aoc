package aoc2019.intcode.io

interface IO {
    fun read(): Int
    fun write(out: Int)
}
