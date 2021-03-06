package aoc2020.oneliner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import library.tuple.Pair;
import library.tuple.Tuple;

public class Day3 {

	public static long part1() throws IOException {
		return ((Function<List<String>, Long>) list -> Stream.iterate(0, x -> x + 1).limit(list.size())
				.map(i -> Tuple.of(i, list.get(i)))
				.filter(p -> p.getSecond().charAt((p.getFirst() * 3) % p.getSecond().length()) == '#')
				.count())
						.apply(Files.lines(Path.of("src/main/resources/aoc2020/day3.txt"))
								.collect(Collectors.toList()));
	}

	public static long part2() throws IOException {
		return ((Function<List<Pair<Integer, String>>, Long>) rows -> List.of(
				Tuple.of(1, 1),
				Tuple.of(3, 1),
				Tuple.of(5, 1),
				Tuple.of(7, 1),
				Tuple.of(1, 2)).stream()
				.map(path -> rows.stream()
						.filter(row -> (row.getFirst() % path.getSecond()) == 0
								&& row.getSecond()
										.charAt(((row.getFirst() / path.getSecond()) * path.getFirst())
												% row.getSecond().length()) == '#')
						.count())
				.reduce(1L, (a, b) -> a * b)).apply(
						((Function<List<String>, List<Pair<Integer, String>>>) data -> Stream
								.iterate(0, x -> x + 1).limit(data.size())
								.map(i -> Tuple.of(i, data.get(i)))
								.collect(Collectors.toList()))
										.apply(Files.lines(Path.of("src/main/resources/aoc2020/day3.txt"))
												.collect(Collectors.toList())));
	}

	public static void main(String[] args) throws IOException {
		System.out.println(part1());
		System.out.println(part2());
	}
}
