package aoc2020.day13;

import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class Main {

    public static long part1(long earliest, String[] data) {
        List<Integer> ids = Arrays.stream(data)
                .filter(s -> !s.equals("x"))
                .map(Integer::parseInt)
                .collect(Collectors.toList());

        long min = Long.MAX_VALUE;
        int minId = -1;
        for (Integer i : ids) {
            long value = earliest / i;
            if (earliest % i != 0) value++;
            value *= i;
            if (value < min) {
                min = value;
                minId = i;
            }
        }
        return (min - earliest) * minId;
    }

    static class Bus implements Comparable<Bus> {
        int id;
        int index;

        public Bus(int id, int index) {
            this.id = id;
            this.index = index;
        }

        @Override
        public String toString() {
            return id + ": " + index;
        }

        @Override
        public int compareTo(Bus o) {
            return Integer.compare(id, o.id);
        }
    }

    public static long part2(String[] data) {
        List<Bus> buses = new ArrayList<>();
        for (int i = 0; i < data.length; i++) {
            if (!data[i].equals("x")) buses.add(new Bus(Integer.parseInt(data[i]), i));
        }

        long p = buses.get(0).id;
        long result = buses.get(0).index;
        for (int i = 1; i < buses.size(); i++) {
            Bus bus = buses.get(i);
            long k = 0;
            while (((k * p) + result + bus.index) % bus.id != 0) {
                k++;
            }
            result += (k * p);
            p *= bus.id;
        }

        return result % p;
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year.AOC_2020, "day13.txt");

        long earliest = Long.parseLong(sc.nextLine());
        String[] data = sc.nextLine().split(",");

        System.out.println(part1(earliest, data));

        System.out.println(part2(data));
    }

}
