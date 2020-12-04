package aoc2020.oneliner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Day1 {

	public static int part1() throws IOException {
		return ((Function<List<Integer>, Integer>) s -> s.stream()
				.filter(i -> s.stream().anyMatch(j -> j == 2020 - i))
				.map(i -> i * (2020 - i))
				.findAny().get()).apply(Files.lines(Path.of("src/main/resources/aoc2020/day1.txt"))
						.map(Integer::parseInt).collect(Collectors.toList()));
	}

	public static int part2() throws IOException {
		return ((Function<List<Integer>, Integer>) s -> s.stream()
				.flatMap(a -> s.stream()
						.flatMap(b -> s.stream()
								.filter(c -> a + b + c == 2020)
								.map(c -> a * b * c)))
				.findAny().get()).apply(Files.lines(Path.of("src/main/resources/aoc2020/day1.txt"))
						.map(Integer::parseInt).collect(Collectors.toList()));
	}

	public static void main(String[] args) throws IOException {
		System.out.println(part1());
		System.out.println(part2());
	}

}
