package aoc2019.intcode.io

open class DefaultIO(settings: Array<Long> = emptyArray()) : IO(settings) {
    override suspend fun provide(): Long {
        print("Please enter the input: ")
        return readln().toLong()
    }

    override suspend fun write(out: Long) {
        println(out)
    }
}
