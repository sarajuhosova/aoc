package aoc2021.day03;

import library.DataType;
import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.List;

public class Diagnostics {

    private static int powerConsumption(List<String> data) {
        int gamma = gamma(data);
        return gamma * epsilon(gamma, data.get(0).length());
    }

    private static int epsilon(int gamma, int length) {
        String epsilon = Long.toBinaryString(~gamma);
        epsilon = epsilon.substring(epsilon.length() - length);
        return Integer.parseInt(epsilon, 2);
    }
    
    private static int gamma(List<String> data) {
        StringBuilder gamma = new StringBuilder();
        for (int i = 0; i < data.get(0).length(); i++) {
            int ones = 0;
            int zeros = 0;
            for (String info : data) {
                if (info.charAt(i) == '1') ones++;
                else zeros++;
            }
            gamma.append((ones >= zeros) ? "1" : "0");
        }
        return Integer.parseInt(gamma.toString(), 2);
    }

    private static char getCriterion(List<String> data, boolean most, int i) {
        int ones = 0;
        int zeros = 0;
        for (String info : data) {
            if (info.charAt(i) == '1') ones++;
            else zeros++;
        }

        if (most) return (ones >= zeros) ? '1' : '0';
        else return (ones < zeros) ? '1' : '0';
    }

    private static int filter(List<String> data, boolean most, int i) {
        if (data.size() == 1) {
            return Integer.parseInt(data.get(0), 2);
        }

        char criterion = getCriterion(data, most, i);

        List<String> filtered = new ArrayList<>();
        for (String datum : data) {
            if (datum.charAt(i) == criterion)
                filtered.add(datum);
        }
        return filter(filtered, most, i + 1);
    }

    private static int filter(List<String> data, boolean most) {
        return filter(data, most, 0);
    }

    private static int oxygenAndCO2(List<String> data) {
        return filter(data, true) * filter(data, false);
    }

    public static void main(String[] args) {
        // load data
        List<String> data = Input.collectDataToList(
                Input.readData(Year._2021, "day03.txt"), DataType.WORD
        );

        // Part 1
        System.out.println(powerConsumption(data));

        // Part 2
        System.out.println(oxygenAndCO2(data));
    }

}
