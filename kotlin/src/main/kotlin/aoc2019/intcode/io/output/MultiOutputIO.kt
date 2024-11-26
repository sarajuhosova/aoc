package aoc2019.intcode.io.output

import aoc2019.intcode.io.IO

open class MultiOutputIO(private val output: OutputList): IO() {

    override suspend fun provide(): Long {
        TODO("Not yet implemented")
    }

    override suspend fun write(out: Long) {
        output.add(out)
    }

}
