package aoc2020.day15;

import library.DataType;
import library.Year;
import library.io.Input;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NumberGames {

    public static int ithNumber(int I, List<Integer> data) {
        Map<Integer, Integer> numbers = new HashMap<>();

        int index = 0;
        int last = -1;
        for (int i : data){
            if (last != -1) numbers.put(last, index);
            last = i;
            index++;
        }

        while (index < I) {
            int l = last;
            if (!numbers.containsKey(l)) {
                last = 0;
            } else {
                last = index - numbers.get(l);
            }
            numbers.put(l, index);
            index++;
        }

        return last;
    }

    public static void main(String[] args) {
        List<Integer> data = Input.collectDataToList(
                Input.readData(Year.AOC_2020, "day15.txt", ","),
                DataType.INT
        );

        // Part 1
        System.out.println(ithNumber(2020, data));

        // Part 2
        System.out.println(ithNumber(30000000, data));
    }

}
