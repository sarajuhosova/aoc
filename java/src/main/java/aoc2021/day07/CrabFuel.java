package aoc2021.day07;

import library.Year;
import library.io.Input;

import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class CrabFuel {

    private static List<Integer> read() {
        Scanner sc = Input.openFile(Year._2021, "day07.txt");

        return Arrays.stream(sc.nextLine().split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
    }

    private static long countConstantFuel(List<Integer> data, int goal) {
        long total = 0;
        for (int pos : data)
            total += Math.abs(pos - goal);
        return total;
    }

    private static long countIncreasingFuel(List<Integer> data, int goal) {
        long total = 0;
        for (int pos : data) {
            int n = Math.abs(pos - goal);
            total += ((long) n * (n + 1)) / 2;
        }
        return total;
    }

    private static long getLeastFuel(List<Integer> data,
                                     BiFunction<List<Integer>, Integer, Long> fun) {
        long min = Long.MAX_VALUE;
        int start = data.stream().mapToInt(x -> x).min().getAsInt();
        int end = data.stream().mapToInt(x -> x).max().getAsInt();
        for (int i = start; i < end; i++) {
            long fuel = fun.apply(data, i);
            if (fuel < min) min = fuel;
        }
        return min;
    }

    public static void main(String[] args) {
        // load data
        List<Integer> data = read();

        // part 1
        System.out.println(getLeastFuel(data, CrabFuel::countConstantFuel));

        // part 2
        System.out.println(getLeastFuel(data, CrabFuel::countIncreasingFuel));
    }

}
