package aoc2022.day13

import library.Year
import library.readData
import java.util.Stack

abstract class Component()

class Number(
    val number: Int
): Component()

class Nested(
    val list: List<Component>
): Component()

enum class Decision(val value: Int) {
    CORRECT(-1),
    INCORRECT(1),
    NOT_DETERMINED(0);
}

fun compareNumbers(left: Number, right: Number): Decision {
    if (left.number < right.number) return Decision.CORRECT
    if (left.number > right.number) return Decision.INCORRECT
    return Decision.NOT_DETERMINED
}

fun compareNested(left: Nested, right: Nested): Decision {
    for (pair in left.list.zip(right.list)) {
        val decision = compareComponents(pair.first, pair.second)
        if (decision == Decision.NOT_DETERMINED) continue
        return decision
    }
    if (left.list.size < right.list.size) return Decision.CORRECT
    if (right.list.size < left.list.size) return Decision.INCORRECT
    return Decision.NOT_DETERMINED
}

fun compareComponents(left: Component, right: Component): Decision {
    if (left is Number) {
        if (right is Number) {
            return compareNumbers(left, right)
        }
        return compareNested(Nested(listOf(left)), right as Nested)
    }
    if (right is Number) {
        return compareNested(left as Nested, Nested(listOf(right)))
    }
    return compareNested(left as Nested, right as Nested)
}

class ComponentComparator : Comparator<Component> {
    override fun compare(left: Component?, right: Component?): Int {
        if(left == null || right == null){
            return 0;
        }
        return compareComponents(left, right).value
    }
}

fun parse(line: String): Component {
    val stack = Stack<MutableList<Component>>()

    var i = 0
    var hasInt = false
    for (c in line) {
        when (c) {
            '[' -> stack.push(mutableListOf())
            ']' -> {
                if (hasInt) {
                    stack.peek().add(Number(i))
                    i = 0
                    hasInt = false
                }
                val bye = Nested(stack.pop())
                if (stack.isEmpty()) return bye
                stack.peek().add(bye)
            }
            in '0'..'9' -> {
                i = (i * 10) + (c - '0')
                hasInt = true
            }
            else -> {
                if (hasInt) {
                    stack.peek().add(Number(i))
                    i = 0
                    hasInt = false
                }
            }
        }
    }

    return Nested(stack.pop())
}


fun parse(data: List<String>): Pair<Component, Component> =
    Pair(parse(data[0]), parse(data[1]))

fun toPairs(packets: List<Component>): List<Pair<Component, Component>> =
    when (packets.size) {
        0 -> listOf()
        else -> listOf(Pair(packets[0], packets[1])) + toPairs(packets.drop(2))
    }

fun findIndex(packets: List<Pair<Int, Component>>, component: Component): Int =
    packets.find { it.second == component }!!.first

fun main() {
    val packets = readData(Year._2022, "day13.txt")
        .filter { it != "" }.map { parse(it) }

    // part 1
    val pairs = toPairs(packets)
    val inOrder = pairs.mapIndexed { i, pair -> Pair(i + 1, pair) }
        .filter { compareComponents(it.second.first, it.second.second) == Decision.CORRECT }
        .map { it.first }
    println(inOrder.sum())

    // part 2
    val dividers = listOf("[[2]]", "[[6]]").map { parse(it) }
    val sorted = packets.plus(dividers)
        .sortedWith(ComponentComparator())
        .mapIndexed { index, component -> Pair(index + 1, component) }
    println(dividers.map { findIndex(sorted, it) }.fold(1) { acc, i -> acc * i })
}
