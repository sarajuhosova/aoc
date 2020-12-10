package aoc2020.day10;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.List;
import java.util.stream.Collectors;

public class JoltageAdapting {

    public static Pair<Integer, Integer> linking(List<Integer> data) {
        int one = 0;
        int three = 1;

        int prev = 0;
        for (Integer i : data) {
            if (i - prev == 1) one++;
            if (i - prev == 3) three++;
            prev = i;
        }

        return new Pair<>(one, three);
    }

    public static long possibilities(List<Integer> data) {
        long[] dynamic = new long[data.size()];
        dynamic[data.size() - 1] = 1;

        for (int i = data.size() - 1; i >= 0; i--) {
            int current = data.get(i);
            int j = i + 1;
            while (j < data.size() && data.get(j) <= current + 3) {
                dynamic[i] += dynamic[j];
                j++;
            }
        }

        int i = 0;
        long total = 0;
        while (data.get(i) <= 3) {
            total += dynamic[i];
            i++;
        }

        return total;
    }

    public static void main(String[] args) {
        // Read and Sort data
        List<Integer> data = Input.readData(Year.AOC_2020, "day10.txt")
                .map(Integer::parseInt)
                .sorted()
                .collect(Collectors.toList());

        // Part 1
        Pair<Integer, Integer> diffs = linking(data);
        System.out.println(diffs.getFirst() * diffs.getSecond());

        // Part 2
        System.out.println(possibilities(data));
    }

}
