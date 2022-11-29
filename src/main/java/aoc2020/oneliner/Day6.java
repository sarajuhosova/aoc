package aoc2020.oneliner;

import java.io.IOException;
import java.util.Arrays;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

public class Day6 {

	public static long part1() {
		return Arrays.stream(Input.readData(Year._2020, "day6.txt")
				.reduce("", (a, b) -> a += b + "\n")
				.split("\n\n"))
				.map(p -> p.replace("\n", ""))
				.mapToLong(s -> s.chars().mapToLong(c -> (long) c).distinct().count())
				.sum();
	}

	public static long part2() {
		return Arrays.stream(Input.readData(Year._2020, "day6.txt")
				.reduce("", (a, b) -> a += b + "\n")
				.split("\n\n"))
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

	public static void main(String[] args) throws IOException {
		System.out.println(part1());
		System.out.println(part2());
	}
}
