package aoc2021.day01;

import library.DataType;
import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.List;

public class IncreasingDepths {

    private static int countIncreases(List<Integer> data) {
        int total = 0;
        for (int i = 1; i < data.size(); i++) {
            if (data.get(i) > data.get(i - 1)) total++;
        }
        return total;
    }

    private static int countIncreasesWithWindow(List<Integer> data) {
        List<Integer> result = new ArrayList<>();
        for (int i = 0; i < data.size() - 2; i++) {
            result.add(data.get(i) + data.get(i + 1) + data.get(i + 2));
        }
        return countIncreases(result);
    }

    public static void main(String[] args) {
        System.out.println("Hello, Advent of Code 2021!");

        // load data
        List<Integer> data = Input.collectDataToList(
                Input.readData(Year._2021, "day01.txt"), DataType.INT
        );

        // Part 1
        System.out.println(countIncreases(data));

        // Part 2
        System.out.println(countIncreasesWithWindow(data));
    }

}
