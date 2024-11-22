package aoc2019.intcode

open class IntCodeException() : Exception()
class ProgramHaltedException() : IntCodeException()
class UnknownInstructionException() : IntCodeException()
