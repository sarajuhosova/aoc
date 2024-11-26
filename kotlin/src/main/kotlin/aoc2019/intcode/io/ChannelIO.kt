package aoc2019.intcode.io

import kotlinx.coroutines.channels.Channel

class ChannelIO(
    settings: Array<Long>,
    private val provider: Channel<Long>,
    private val sender: Channel<Long>
): IO(settings) {
    override suspend fun provide(): Long = provider.receive()

    override suspend fun write(out: Long) {
        sender.send(out)
    }
}
