package aoc2019.intcode.expections

class UnknownInstructionException(opcode: Int) : IntCodeException("Opcode ${opcode} is unknown")
