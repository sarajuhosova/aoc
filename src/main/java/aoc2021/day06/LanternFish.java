package aoc2021.day06;

import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

public class LanternFish {

    private static List<Integer> read() {
        Scanner sc = Input.openFile(Year._2021, "day06.txt");

        return Arrays.stream(sc.nextLine().split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
    }

    private static List<Long> preprocess(List<Integer> data, int cycle) {
        List<Long> fish = new ArrayList<>();
        for (int i = 0; i < cycle; i++) {
            fish.add((long) 0);
        }
        for (int f : data) {
            fish.set(f, fish.get(f) + 1);
        }
        return fish;
    }

    private static long countFish(List<Long> fish) {
        long sum = 0;
        for (long f : fish) sum += f;
        return sum;
    }

    private static long countGrowth(List<Integer> data, int cycle, int restart, int days) {
        List<Long> fish = preprocess(data, cycle);
        for (int d = 0; d < days; d++) {
            long temp = fish.remove(0);
            fish.set(restart, fish.get(restart) + temp);
            fish.add(temp);
        }
        return countFish(fish);
    }

    public static void main(String[] args) {
        // load data
        List<Integer> data = read();

        // part 1
        System.out.println(countGrowth(data, 9, 6, 80));

        // part 2
        System.out.println(countGrowth(data, 9, 6, 256));
    }

}
