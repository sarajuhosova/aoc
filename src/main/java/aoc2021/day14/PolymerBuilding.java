package aoc2021.day14;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class PolymerBuilding {

    private static Pair<String, Map<String, Character>> read() {
        Scanner sc = Input.openFile(Year._2021, "day14.txt");

        String start = sc.nextLine();
        sc.nextLine();

        Map<String, Character> map = new HashMap<>();
        while (sc.hasNextLine()) {
            String[] data = sc.nextLine().split(" -> ");
            map.put(data[0], data[1].charAt(0));
        }
        return new Pair<>(start, map);
    }

    private static <T> void addToMap(Map<T, Long> map, T key, long amount) {
        if (map.containsKey(key)) map.put(key, map.get(key) + amount);
        else map.put(key, amount);
    }

    private static <T> void addToMap(Map<T, Long> map, T key) {
        addToMap(map, key, 1L);
    }

    private static Map<String, Long> countPairs(String string) {
        Map<String, Long> map = new HashMap<>();
        for (int i = 0; i < string.length() - 1; i++) {
            addToMap(map, string.substring(i, i + 2));
        }
        return map;
    }

    private static Map<Character, Long> countChars(String string) {
        Map<Character, Long> counts = new HashMap<>();
        for (int i = 0; i < string.length(); i++) {
            addToMap(counts, string.charAt(i));
        }
        return counts;
    }
    
    private static void updatePair(Map<String, Long> start,
                                   Map<Character, Long> counts,
                                   String string, char c, long amount) {
        addToMap(start, "" + string.charAt(0) + c, amount);
        addToMap(start, "" + c + string.charAt(1), amount);
        addToMap(counts, c, amount);
    }

    private static Map<String, Long> step(Map<String, Long> start,
                                          Map<Character, Long> counts,
                                          Map<String, Character> map) {
        Map<String, Long> next = new HashMap<>();
        for (String string : start.keySet()) {
            updatePair(next, counts, string, map.get(string), start.get(string));
        }
        return next;
    }

    private static long build(Map<String, Long> start,
                              Map<Character, Long> counts,
                              Map<String, Character> map,
                              int steps) {
        for (int i = 0; i < steps; i++) {
            start = step(start, counts, map);
        }
        return getDiff(counts);
    }

    private static long getDiff(Map<Character, Long> counts) {
        Pair<Character, Long> max = new Pair<>('-', Long.MIN_VALUE);
        Pair<Character, Long> min = new Pair<>('-', Long.MAX_VALUE);

        for (Character key : counts.keySet()) {
            long value = counts.get(key);
            if (value > max.getSecond()) max = new Pair<>(key, value);
            if (value < min.getSecond()) min = new Pair<>(key, value);
        }

        return max.getSecond() - min.getSecond();
    }

    public static void main(String[] args) {
        // load data
        Pair<String, Map<String, Character>> pair = read();
        Map<String, Character> map = pair.getSecond();

        // Part 1
        Map<String, Long> start = countPairs(pair.getFirst());
        Map<Character, Long> counts = countChars(pair.getFirst());
        System.out.println(build(start, counts, map, 10));

        // Part 2
        start = countPairs(pair.getFirst());
        counts = countChars(pair.getFirst());
        System.out.println(build(start, counts, map, 40));
    }

}
