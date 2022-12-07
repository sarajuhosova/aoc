package aoc2022.day07

import library.Year
import library.head
import library.readData
import java.util.*

const val totalSize  = 70000000
const val updateSize = 30000000

abstract class Component {
    abstract val name: String
}

data class File(
    override val name: String,
    val size: Long
): Component()

data class Directory(
    override val name: String,
    val children: MutableList<Component> = mutableListOf(),
    val parent: Directory? = null,
    var size: Long = 0L
): Component()

fun parse(data: List<String>): Directory {
    val root = Directory("/")

    var current = root
    for (line in data) {
        val split = line.split(" ")
        if (split[0] == "$") {
            if (split[1] == "cd") {
                current = when (split[2]) {
                    ".." -> current.parent!!
                    "/"  -> root
                    else -> current.children.find { it.name == split[2] }!! as Directory
                }
            } else if (split[1] == "ls") {
                continue
            }
        } else {
            if (split[0] == "dir") {
                current.children.add(Directory(split[1], parent = current))
            } else {
                current.children.add(File(split[1], split[0].toLong()))
            }
        }
    }

    return root
}

fun getDirectSize(dir: Directory): Long =
    dir.children
        .filterIsInstance<File>()
        .sumOf { it.size }

fun getSize(dir: Directory): Long {
    val dirs = dir.children.filterIsInstance<Directory>()
    return when (dirs.size) {
        0 -> getDirectSize(dir)
        else -> getDirectSize(dir) + dirs.sumOf { getSize(it) }
    }
}

fun countDirs(root: Directory): List<Directory> {
    val stack = Stack<Directory>()
    stack.push(root)

    var result = mutableListOf<Directory>()
    while (!stack.empty()) {
        val dir = stack.pop()

        dir.size = getSize(dir)
        result.add(dir)

        dir.children
            .filterIsInstance<Directory>()
            .forEach { stack.push(it) }
    }

    return result
}

fun main() {
    val root = parse(readData(Year._2022, "day07.txt"))

    val dirs = countDirs(root).sortedBy { it.size }
    val leftoverSize = totalSize - root.size

    println(dirs.takeWhile { it.size <= 100000 }.sumOf { it.size })
    println(dirs.dropWhile { (leftoverSize + it.size) < updateSize }.head().size)
}
