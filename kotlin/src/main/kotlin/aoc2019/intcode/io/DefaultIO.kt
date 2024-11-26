package aoc2019.intcode.io

open class DefaultIO(settings: Array<Int> = emptyArray()) : IO(settings) {
    override suspend fun provide(): Int {
        print("Please enter the input: ")
        return readln().toInt()
    }

    override suspend fun write(out: Int) {
        println(out)
    }
}
