package aoc2020.day08;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

public class InfiniteLooping {

	static Map<String, Instruction> map = Instruction.map();

	static class Command {
		Instruction opcode;
		int value;
		boolean visited = false;

		public Command(Instruction opcode, int value) {
			this.opcode = opcode;
			this.value = value;
		}
	}

	private static int part1(List<Command> data) {
		int acc = 0;
		boolean[] visited = new boolean[data.size()];
		for (int i = 0; i < data.size(); i++) {
			Command c = data.get(i);
			if (visited[i])
				return acc;
			visited[i] = true;
			switch (c.opcode) {
				case ACC:
					acc += c.value;
					break;
				case JMP:
					i += c.value - 1;
					break;
				case NOP:
				default:
					break;
			}
		}
		return acc;
	}

	private static void swap(Command c) {
		if (c.opcode == Instruction.JMP)
			c.opcode = Instruction.NOP;
		else if (c.opcode == Instruction.NOP)
			c.opcode = Instruction.JMP;
	}

	private static Integer loopDetection(List<Command> data) {
		int acc = 0;
		boolean[] visited = new boolean[data.size()];
		for (int i = 0; i < data.size(); i++) {
			Command c = data.get(i);
			if (visited[i])
				return null;
			visited[i] = true;
			switch (c.opcode) {
				case ACC:
					acc += c.value;
					break;
				case JMP:
					i += c.value - 1;
					break;
				case NOP:
				default:
					break;
			}
		}
		return acc;
	}

	private static Integer part2(List<Command> data) {
		for (Command c : data) {
			if (c.opcode == Instruction.ACC)
				continue;
			swap(c);
			Integer result = loopDetection(data);
			if (result != null)
				return result;
			swap(c);
		}
		return null;
	}

	public static void main(String[] args) {
		List<Command> data = Input.readData(Year._2020, "day08.txt")
				.map(s -> s.split(" "))
				.map(a -> new Command(map.get(a[0]), Integer.parseInt(a[1])))
				.collect(Collectors.toList());

		System.out.println(part1(data));
		System.out.println(part2(data));
	}

}
