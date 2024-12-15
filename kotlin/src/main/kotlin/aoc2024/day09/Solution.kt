package aoc2024.day09

import library.Year
import library.readFirst

abstract class DiskSpace(var size: Int) {
    abstract fun toCheck(index: Int): Long
}
class File(val id: Int, size: Int): DiskSpace(size) {
    override fun toCheck(index: Int): Long =
        (0 until size).sumOf { (index + it) * id.toLong() }

    override fun toString(): String {
        return id.digitToChar().toString().repeat(size)
    }
}
class Space(size: Int): DiskSpace(size) {
    override fun toCheck(index: Int): Long = 0L

    override fun toString(): String {
        return ".".repeat(size)
    }
}

typealias Disk = List<DiskSpace>

fun parse(diskMap: String): Disk {
    val result = mutableListOf<DiskSpace>()

    var space = false
    var id = 0
    for (c in diskMap) {
        if (space) {
            result.add(Space(c.digitToInt()))
            space = false
        } else {
            result.add(File(id, c.digitToInt()))
            id++
            space = true
        }
    }

    return result
}

fun Disk.size(): Int = this.sumOf { it.size }
fun Disk.print(): String =
    this.map { it.toString() }.reduce { acc, next -> acc + next }
fun Disk.lastFile(index: Int = size): File =
    (if (index % 2 == 0) this[index] else this[index - 1]) as File
fun Disk.copy(): Disk = this.map { it }

fun Disk.indexOf(id: Int): Int {
    for (index in this.size - 1 downTo 0) {
        val place = this[index]
        if (place is File && place.id == id) return index
    }
    return -1
}

fun Disk.checksum(): Long {
    var result = 0L
    var index = 0
    for (place in this) {
        result += place.toCheck(index)
        index += place.size
    }
    return result
}

fun Disk.rearrangeFragmented(): Disk {
    val disk = mutableListOf<DiskSpace>()

    var start = 0
    var end = this.size()
    var last: Pair<Int, File> = Pair(this.size - 1, this.lastFile())
    for (place in this) {
        if (start >= end) break
        if (place is File) {
            if (place.size > (end - start))
                disk.add(File(place.id, end - start))
            else disk.add(place)
        }
        if (place is Space) {
            fun move(size: Int) {
                if (last.second.size == size) {
                    disk.add(last.second)
                    last = Pair(last.first - 2, this.lastFile(last.first - 2))
                    end -= (size + this[last.first + 1].size)
                } else if (last.second.size > size) {
                    disk.add(File(last.second.id, size))
                    last = Pair(last.first, File(last.second.id, last.second.size - size))
                    end -= size
                } else /* (last.second.size < size) */ {
                    disk.add(last.second)
                    end -= (last.second.size + this[last.first - 1].size)
                    if (start >= end) return
                    val nextSize = size - last.second.size
                    last = Pair(last.first - 2, this.lastFile(last.first - 2))
                    move(nextSize)
                }
            }

            move(place.size)
        }

        start += place.size
    }
    
    return disk
}

fun Disk.firstSpaceIndex(size: Int): Int? {
    for (i in indices) {
        if (this[i] is Space && this[i].size >= size) return i
    }
    return null
}

fun Disk.rearrange(): Disk {
    fun MutableList<DiskSpace>.rearrange() {
        val files = this.filterIsInstance<File>().sortedByDescending { it.id }

        for (file in files) {
            val index = this.indexOf(file.id)

            if (index == 0) break

            val spaceIndex = this.firstSpaceIndex(file.size)
            if (spaceIndex == null || spaceIndex >= index) continue

            this.removeAt(index)
            this.add(index, Space(file.size))

            val space = this.removeAt(spaceIndex)
            this.add(spaceIndex, file)
            if (file.size < space.size) this.add(spaceIndex + 1, Space(space.size - file.size))
        }
    }

    val copy = this.copy().toMutableList()
    copy.rearrange()
    return copy
}

fun main() {
    val disk = parse(readFirst(Year._2024, 9))

    println(disk.rearrangeFragmented().checksum())
    println(disk.rearrange().checksum())
}
