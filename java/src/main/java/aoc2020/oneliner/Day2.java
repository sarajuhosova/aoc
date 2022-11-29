package aoc2020.oneliner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import library.tuple.Tuple;

public class Day2 {

	public static long part1() throws IOException {
		return Files.lines(Path.of("src/main/resources/aoc2020/day2.txt"))
				.map(l -> l.split(" |-"))
				.map(a -> Tuple.of(
						Integer.parseInt(a[0]),
						Integer.parseInt(a[1]),
						a[3].chars().filter(c -> c == (int) a[2].charAt(0)).count()))
				.filter(t -> t.getThird() >= t.getFirst() && t.getThird() <= t.getSecond())
				.count();
	}

	public static long part2() throws IOException {
		return Files.lines(Path.of("src/main/resources/aoc2020/day2.txt"))
				.map(l -> l.split(" |-"))
				.map(a -> Tuple.of(
						Integer.parseInt(a[0]),
						Integer.parseInt(a[1]),
						a[2].charAt(0),
						a[3]))
				.filter(q -> (q.getFourth().charAt(q.getFirst() - 1) == q.getThird())
						^ (q.getFourth().charAt(q.getSecond() - 1) == q.getThird()))
				.count();
	}

	public static void main(String[] args) throws IOException {
		System.out.println(part1());
		System.out.println(part2());
	}

}
