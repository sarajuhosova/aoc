package aoc2020.day8;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

public enum Instruction {
	JMP(
		"jmp"
	),
	ACC(
		"acc"
	),
	NOP(
		"nop"
	);

	String string;

	Instruction(String string) {
		this.string = string;
	}

	public String getString() {
		return string;
	}

	public static Map<String, Instruction> map() {
		return Arrays.stream(Instruction.values())
				.collect(Collectors.toMap(Instruction::getString, k -> k));
	}
}
