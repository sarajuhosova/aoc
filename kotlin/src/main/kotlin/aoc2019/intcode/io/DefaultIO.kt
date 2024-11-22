package aoc2019.intcode.io

open class DefaultIO : IO {
    override fun read(): Int {
        print("Please enter the input: ")
        return readln().toInt()
    }

    override fun write(out: Int) {
        println(out)
    }
}
