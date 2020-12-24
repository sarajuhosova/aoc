package aoc2020.day23;

import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CrabbyCups {

    private static final int MILLION = 1000000;
    private static int max;

    private static Map<Integer, Integer> readSmallCircle(String data) {
        Map<Integer, Integer> circle = new HashMap<>();

        for (int i = 0; i < data.length() - 1; i++) {
            circle.put(data.charAt(i) - '0', data.charAt(i + 1) - '0');
        }
        circle.put(data.charAt(data.length() - 1) - '0', data.charAt(0) - '0');

        return circle;
    }

    private static Map<Integer, Integer> readLargeCircle(String data) {
        Map<Integer, Integer> circle = new HashMap<>();

        int max = Integer.MIN_VALUE;
        for (int i = 0; i < data.length() - 1; i++) {
            int num = data.charAt(i) - '0';
            circle.put(num, data.charAt(i + 1) - '0');
            max = Math.max(max, num);
        }

        int num = data.charAt(data.length() - 1) - '0';
        max = Math.max(max, num) + 1;
        circle.put(num, max);

        for (int i = max; i < MILLION; i++) {
            circle.put(i, i + 1);
        }
        circle.put(MILLION, data.charAt(0) - '0');

        return circle;
    }

    private static void executeMove(Map<Integer, Integer> circle, int first) {
        List<Integer> next = new ArrayList<>();
        int n = first;
        for (int i = 0; i < 3; i++) {
            n = circle.get(n);
            next.add(n);
        }

        int smaller = first - 1;
        if (smaller < 1)  smaller = max;
        while (next.contains(smaller)) {
            smaller--;
            if (smaller < 1) smaller = max;
        }

        circle.put(first, circle.get(next.get(2)));
        int after = circle.get(smaller);
        circle.put(smaller, next.get(0));
        circle.put(next.get(2), after);
    }

    private static void play(Map<Integer, Integer> circle, int first, int amount) {
        max = circle.keySet().stream().mapToInt(i -> i).max().getAsInt();
        for (int i = 0; i < amount; i++) {
            executeMove(circle, first);
            first = circle.get(first);
        }
    }

    private static long findStars(Map<Integer, Integer> circle) {
        int first = circle.get(1);
        long second = circle.get(first);

        return first * second;
    }

    public static void main(String[] args) {
        String data = Input.openFile(Year.AOC_2020, "day23.txt").nextLine();

        Map<Integer, Integer> smallCircle = readSmallCircle(data);
        play(smallCircle, data.charAt(0) - '0',100);
        System.out.println(collect(smallCircle));

        Map<Integer, Integer> largeCircle = readLargeCircle(data);
        play(largeCircle, data.charAt(0) - '0', 10 * MILLION);
        System.out.println(findStars(largeCircle));
    }

    private static String collect(Map<Integer, Integer> circle) {
        StringBuilder builder = new StringBuilder();
        int current = circle.get(1);
        while (current != 1) {
            builder.append(current);
            current = circle.get(current);
        }
        return builder.toString();
    }

}
