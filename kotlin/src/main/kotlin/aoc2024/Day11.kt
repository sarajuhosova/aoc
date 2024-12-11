package aoc2024

import library.Year
import library.readFirst

val ZEROS = mutableListOf<Long>()

abstract class Stone {
    abstract fun size(date: Int): Long
}
class ZeroStone(private val dob: Int) : Stone() {
    override fun size(date: Int): Long = ZEROS[date - dob]
}
class NormalStone(private val engraving: Long): Stone() {
    val string = engraving.toString()
    
    override fun size(date: Int): Long = 1L

    fun multiply(): NormalStone = NormalStone(engraving * 2024)
    
    fun isEven(): Boolean = string.length % 2 == 0
    fun split(dob: Int): Pair<Stone, Stone> = Pair(
        string.substring(0, string.length / 2).toLong().toStone(dob),
        string.substring(string.length / 2).toLong().toStone(dob)
    )
}

fun Long.toStone(birth: Int = 0): Stone =
    if (this == 0L) ZeroStone(birth) else NormalStone(this)

fun blinkNaive(stones: MutableList<Long>) {
    for (index in stones.lastIndex downTo 0) {
        val engraving = stones[index]
        if (engraving == 0L) stones[index] = 1
        else {
            val string = engraving.toString()
            if (string.length % 2 == 0) {
                stones[index] = string.substring(0, string.length / 2).toLong()
                stones.add(index + 1, string.substring(string.length / 2).toLong())
            }
            else stones[index] = stones[index] * 2024
        }
    }
}

fun blink(stones: MutableList<Stone>, dob: Int) {
    for (index in stones.lastIndex downTo 0) {
        val stone = stones[index]
        if (stone is NormalStone) {
            if (stone.isEven()) {
                val (first, second) = stone.split(dob)
                stones[index] = first
                stones.add(index + 1, second)
            } else {
                stones[index] = stone.multiply()
            }
        }
    }
}

fun main() {
    val stones = readFirst(Year._2024, 11)
        .split(" ").map { it.toLong() }

    // part1
//    val mutable = stones.toMutableList()
//    repeat (25) {
//        blinkNaive(mutable)
//    }
//    println(mutable.size)

    // part2
    val blinks = 75

//    val zeros = mutableListOf<Stone>(NormalStone(1))
//    ZEROS.add(1)
//    ZEROS.add(1)
//    for (i in 1..blinks) {
//        blink(zeros, i)
//        ZEROS.add(zeros.sumOf { it.size(i) })
//    }
//    println(ZEROS.mapIndexed{ index, size -> "$index: $size" }.joinToString("\n"))

    val efficient = stones.map { it.toStone() }.toMutableList()
    for (i in 1..blinks) {
        blink(efficient, i)
    }
    println(efficient.size)

//    println(mutable.size)
}

/*
0
1
2024
20                                                     24
2                               0                      2             4
4048                            1                      4048          8096
40            48                2024                   40            48                80              96
4       0     4       8         20         24          4       0     4       8         8         0     9         6
8096    1     8096    16192     2     0    2     4     8096    1     8096    16192     16192     1     18216     12144
80  96  2024  80  96  32772608  4048  1    4048  8096  80  96  2024  80  96  32772608  32772608  2024  36869184  24579456
8 0 9 6 20 24 8 0 9 6 3277 2608 40 48 2024 40 48 80 96 8 0 9 6 20 24 8 0 9 6 3277 2608 3277 2608 20 24 3686 9184 2457 9456
*/
