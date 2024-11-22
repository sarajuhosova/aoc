package aoc2019.intcode.expections

class UnknownParamModeException(code: Int) : IntCodeException("Code $code is an unknown param mode")
