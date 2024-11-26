package aoc2019

import aoc2019.intcode.Computer
import aoc2019.intcode.io.ChannelIO
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import library.Year
import library.permutations
import library.readFirst
import kotlin.math.max

fun linkAmps(computer: Computer, settings: List<Int>): Int {
    val channels = Array(settings.size) { Channel<Int>(capacity = 1) }
    val ios = Array(settings.size) { index -> ChannelIO(
        arrayOf(settings[index]),
        channels[index],
        channels[(index + 1) % channels.size]
    ) }

    val result = runBlocking {
        val job = launch {
            for (index in settings.indices) {
                launch {
                    computer.run(io = ios[index])
                }
            }
            channels[0].send(0)
        }
        job.join()
        return@runBlocking channels[0].receive()
    }
    channels.forEach { it.close() }

    return result
}

fun findMaxSignal(computer: Computer, settings: List<Int>): Int {
    var maximum = Integer.MIN_VALUE
    for (setting in permutations(settings)) {
        maximum = max(maximum, linkAmps(computer, setting))
    }
    return maximum
}


fun main() {
    val computer = Computer(readFirst(Year._2019, 7))

    // part 1
    println("Maximum: ${findMaxSignal(computer, listOf(0, 1, 2, 3, 4))}")

    // part 2
    println("Maximum with feedback: ${findMaxSignal(computer, listOf(5, 6, 7, 8, 9))}")
}
