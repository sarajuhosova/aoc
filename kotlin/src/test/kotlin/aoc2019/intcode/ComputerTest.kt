package aoc2019.intcode

import library.Year
import library.readFirst

abstract class ComputerTest(private val day: Int) {

    private var computer: Computer? = null

    fun computer(): Computer {
        if (computer == null) {
            computer = Computer(readFirst(Year._2019, day))
        }
        return computer!!
    }

}