package aoc2019.intcode.io

import kotlinx.coroutines.channels.Channel

class ChannelIO(
    settings: Array<Int>,
    private val provider: Channel<Int>,
    private val sender: Channel<Int>
): IO(settings) {
    override suspend fun provide(): Int = provider.receive()

    override suspend fun write(out: Int) {
        sender.send(out)
    }
}
