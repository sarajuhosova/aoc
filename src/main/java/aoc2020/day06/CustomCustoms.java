package aoc2020.day06;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

public class CustomCustoms {

	public static List<String> read() {
		return Arrays.stream(Input.readData(Year._2020, "day06.txt")
				.reduce("", (a, b) -> a += b + "\n")
				.split("\n\n")).collect(Collectors.toList());
	}

	public static long distinctPerGroup(List<String> input) {
		return input.stream()
				.map(p -> p.replace("\n", ""))
				.mapToLong(s -> s.chars().mapToLong(c -> (long) c).distinct().count())
				.sum();
	}

	public static long sharedPerGroup(List<String> input) {
		return input.stream()
				.mapToInt(p -> Arrays.stream(p.split("\n"))
						.map(a -> a.chars().boxed().collect(Collectors.toSet()))
						.reduce("abcdefghijklmnopqrstuvwxyz".chars()
								.boxed().collect(Collectors.toSet()),
								(a, b) -> a.parallelStream()
										.filter(b::contains)
										.collect(Collectors.toSet()))
						.size())
				.sum();
	}

	public static void main(String[] args) {
		List<String> data = read();

		System.out.println(distinctPerGroup(data));
		System.out.println(sharedPerGroup(data));
	}

}
