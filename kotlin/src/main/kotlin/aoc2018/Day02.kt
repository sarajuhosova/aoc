package aoc2018

import library.Year
import library.readData

fun getCounts(id: String): Map<Char, Int> {
    val counts = mutableMapOf<Char, Int>()
    for (c in id) {
        counts[c] = counts.getOrDefault(c, 0) + 1
    }
    return counts
}

fun getChecksum(ids: List<String>): Int {
    val counts = ids.map { getCounts(it) }
    return counts.count { 2 in it.values } * counts.count { 3 in it.values }
}

/**
 * The boxes will have IDs which differ by exactly one character at the same position in both strings.
 * What letters are common between the two correct box IDs?
 */
fun findCommonInMatch(ids: List<String>): String {
    val length = ids[0].length
    for (i in ids.indices) {
        for (j in i + 1 until ids.size) {
            val overlap = ids[i].zip(ids[j])
                .filter { it.first == it.second }
                .map { it.first }
                .joinToString("")
                .toString()
            if (overlap.length == length - 1) return overlap
        }
    }
    return "ERROR"
}

fun main() {
    val ids = readData(Year._2018, 2)

    println(getChecksum(ids))
    println(findCommonInMatch(ids))
}
