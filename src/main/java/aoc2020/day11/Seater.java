package aoc2020.day11;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

public class Seater {

	private static boolean equals(char[][] c1, char[][] c2, int height, int width) {
		for (int i = 1; i < height - 1; i++) {
			for (int j = 1; j < width - 1; j++) {
				if (c1[i][j] != c2[i][j])
					return false;
			}
		}
		return true;
	}

	private static int countOccupied(char[][] seats) {
		int count = 0;
		for (char[] a : seats) {
			for (char c : a) {
				if (c == '#')
					count++;
			}
		}
		return count;
	}

	private static boolean occupy(char[][] seats, int i, int j) {
		return seats[i - 1][j - 1] != '#'
				&& seats[i - 1][j] != '#'
				&& seats[i - 1][j + 1] != '#'
				&& seats[i][j - 1] != '#'
				&& seats[i][j + 1] != '#'
				&& seats[i + 1][j - 1] != '#'
				&& seats[i + 1][j] != '#'
				&& seats[i + 1][j + 1] != '#';
	}

	private static boolean vacate(char[][] seats, int i, int j) {
		int occupied = 0;
		if (seats[i - 1][j - 1] == '#')
			occupied++;
		if (seats[i - 1][j] == '#')
			occupied++;
		if (seats[i - 1][j + 1] == '#')
			occupied++;
		if (seats[i][j - 1] == '#')
			occupied++;
		if (seats[i][j + 1] == '#')
			occupied++;
		if (seats[i + 1][j - 1] == '#')
			occupied++;
		if (seats[i + 1][j] == '#')
			occupied++;
		if (seats[i + 1][j + 1] == '#')
			occupied++;
		return occupied >= 4;
	}

	public static int surrounding(char[][] seats, int height, int width) {
		while (true) {
			char[][] next = new char[height][width];
			for (int i = 0; i < height; i++) {
				for (int j = 0; j < width; j++) {
					char c = seats[i][j];
					if (c == '.')
						next[i][j] = '.';
					else if (c == 'L') {
						if (occupy(seats, i, j))
							next[i][j] = '#';
						else
							next[i][j] = 'L';
					} else if (c == '#') {
						if (vacate(seats, i, j))
							next[i][j] = 'L';
						else
							next[i][j] = '#';
					}
				}
			}

			if (equals(seats, next, height, width))
				return countOccupied(seats);
			seats = next;
		}
	}

	private static boolean occupied(char[][] seats, int i, int j,
			Function<Integer, Integer> iMove, Function<Integer, Integer> jMove) {
		i = iMove.apply(i);
		j = jMove.apply(j);
		while (i >= 0 && i < seats.length && j >= 0 && j < seats[0].length) {
			if (seats[i][j] == '#')
				return true;
			if (seats[i][j] == 'L')
				return false;
			i = iMove.apply(i);
			j = jMove.apply(j);
		}

		return false;
	}

	private static int occupied(char[][] seats, int i, int j) {
		int occupied = 0;

		if (occupied(seats, i, j, x -> x - 1, y -> y - 1))
			occupied++;
		if (occupied(seats, i, j, x -> x - 1, y -> y))
			occupied++;
		if (occupied(seats, i, j, x -> x - 1, y -> y + 1))
			occupied++;
		if (occupied(seats, i, j, x -> x, y -> y - 1))
			occupied++;
		if (occupied(seats, i, j, x -> x, y -> y + 1))
			occupied++;
		if (occupied(seats, i, j, x -> x + 1, y -> y - 1))
			occupied++;
		if (occupied(seats, i, j, x -> x + 1, y -> y))
			occupied++;
		if (occupied(seats, i, j, x -> x + 1, y -> y + 1))
			occupied++;

		return occupied;
	}

	public static int inLineOfSight(char[][] seats, int height, int width) {
		while (true) {
			char[][] next = new char[height][width];
			for (int i = 0; i < height; i++) {
				for (int j = 0; j < width; j++) {
					char c = seats[i][j];
					if (c == '.')
						next[i][j] = '.';
					else if (c == 'L') {
						if (occupied(seats, i, j) == 0)
							next[i][j] = '#';
						else
							next[i][j] = 'L';
					} else if (c == '#') {
						if (occupied(seats, i, j) >= 5)
							next[i][j] = 'L';
						else
							next[i][j] = '#';
					}
				}
			}

			if (equals(seats, next, height, width))
				return countOccupied(seats);
			seats = next;
		}
	}

	private static char[][] parseData(List<List<Character>> data, int width, int height) {
		char[][] seats = new char[height][width];
		for (int i = 0; i < height; i++) {
			seats[i][0] = seats[i][width - 1] = '.';
		}
		for (int i = 0; i < width; i++) {
			seats[0][i] = seats[height - 1][i] = '.';
		}

		for (int i = 0; i < data.size(); i++) {
			for (int j = 0; j < data.get(0).size(); j++) {
				seats[i + 1][j + 1] = data.get(i).get(j);
			}
		}
		return seats;
	}

	public static void main(String[] args) {
		List<List<Character>> data = Input.readData(Year.AOC_2020, "day11.txt")
				.map(s -> s.chars().mapToObj(c -> (char) c).collect(Collectors.toList()))
				.collect(Collectors.toList());

		int height = data.size() + 2;
		int width = data.get(0).size() + 2;
		char[][] seats = parseData(data, width, height);

		System.out.println(surrounding(seats, height, width));
		System.out.println(inLineOfSight(seats, height, width));
	}

}
