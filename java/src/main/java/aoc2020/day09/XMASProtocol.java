package aoc2020.day09;

import java.util.List;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

public class XMASProtocol {

	public static long part1(List<Long> data) {
		for (int i = 25; i < data.size(); i++) {
			long num = data.get(i);
			boolean found = false;
			for (int j = i - 25; j < i; j++) {
				for (int k = j + 1; k < i; k++) {
					if (data.get(j) + data.get(k) == num)
						found = true;
				}
			}
			if (!found)
				return num;
		}

		return -1L;
	}

	public static Pair<Long, Long> part2(List<Long> data, long num) {
		for (int i = 0; i < data.size() - 1; i++) {
			long sum = data.get(i);
			int j = i + 1;
			long min = sum;
			long max = sum;
			while (j < data.size() && sum < num) {
				long n = data.get(j);
				sum += n;
				min = Long.min(min, n);
				max = Long.max(max, n);
				j++;
			}
			if (sum == num)
				return new Pair<>(min, max);
		}
		return null;
	}

	public static void main(String[] args) {
		// read data
		List<Long> values = Input.readData(Year._2020, "day09.txt")
				.map(Long::parseLong)
				.collect(Collectors.toList());

		// Part 1
		long num = part1(values);
		System.out.println(num);

		// Part 2
		Pair<Long, Long> range = part2(values, num);
		System.out.println(range.getFirst() + range.getSecond());
	}

}
