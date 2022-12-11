package aoc2022.day11

import library.*

data class Monkey(
    val items: MutableList<Long>,
    val operation: (Long) -> (Long),
    val throwTo: (Long) -> (Int),
    var inspected: Long = 0L
)

fun parseModulo(data: List<String>): Long =
    data.mapNotNull { Regex("  Test: divisible by ([0-9]+)").find(it) }
        .map { it.destructured.toList().head().toLong() }
        .fold(1L) { acc, l -> acc * l }

fun parseOperation(line: String): (Long) -> Long {
    val data = parseRegex("  Operation: new = (old|[0-9]+) (\\+|\\*) (old|[0-9]+)", line)
    if (data[1] == "*") {
        return { old: Long ->
            (if (data[0] == "old") old else data[0].toLong()) * (if (data[2] == "old") old else data[2].toLong())
        }
    } else {
        return { old: Long ->
            (if (data[0] == "old") old else data[0].toLong()) + (if (data[2] == "old") old else data[2].toLong())
        }
    }
}

fun parseMonkey(lines: List<String>): Pair<Int, Monkey> {
    val index = parseRegex("Monkey ([0-9]+):", lines[0])[0].toInt()
    val starting = lines[1].dropWhile { it != ':' }.drop(2)
        .split(", ").map { it.toLong() }.toMutableList()
    val operation = parseOperation(lines[2])
    val test = parseRegex("  Test: divisible by ([0-9]+)", lines[3])[0].toLong()
    val throwTrue = parseRegex("    If true: throw to monkey ([0-9]+)", lines[4])[0].toInt()
    val throwFalse = parseRegex("    If false: throw to monkey ([0-9]+)", lines[5])[0].toInt()

    return Pair(
        index, Monkey(
            starting,
            operation,
            { if (it % test == 0L) throwTrue else throwFalse }
        )
    )
}

fun parse(data: List<String>): Map<Int, Monkey> =
    parseAsGrouped(data, "") {d -> parseMonkey(d)}.toMap()

fun executeRound(monkeys: Map<Int, Monkey>, divisor: Long, modulo: Long) =
    (0 until monkeys.size).map { monkeys[it]!! }
        .forEach { monkey ->
            monkey.items.map { (monkey.operation(it) / divisor) % modulo }
                .forEach {
                    monkeys[monkey.throwTo(it)]!!.items.add(it)
                }
            monkey.inspected += monkey.items.size.toLong()
            monkey.items.clear()
        }

fun executeRounds(monkeys: Map<Int, Monkey>, rounds: Int, divisor: Long, modulo: Long) {
    for (round in (0 until rounds)) {
        executeRound(monkeys, divisor, modulo)
    }
}

fun getSortedInspected(monkeys: Map<Int, Monkey>): List<Long> =
    monkeys.values.map { it.inspected }.sortedDescending()

fun main() {
    val data = readData(Year._2022, "day11.txt")

    val modulo = parseModulo(data)

    val monkeys = parse(data)
    executeRounds(monkeys, 20, 3, modulo)
    val inspected = getSortedInspected(monkeys)
    println(inspected[0] * inspected[1])

    val monkeysStressed = parse(data)
    executeRounds(monkeysStressed, 10000, 1, modulo)
    val inspectedStressed = getSortedInspected(monkeysStressed)
    println(inspectedStressed[0] * inspectedStressed[1])
}
