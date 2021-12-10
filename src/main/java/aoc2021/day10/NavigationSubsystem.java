package aoc2021.day10;

import library.DataType;
import library.Year;
import library.io.Input;

import java.util.*;

public class NavigationSubsystem {

    private static final Map<Character, Character> pairs = Map.of(
            '(', ')',
            '[', ']',
            '{', '}',
            '<', '>'
    );

    private static final Map<Character, Integer> errorTable = Map.of(
            ')', 3,
            ']', 57,
            '}', 1197,
            '>',25137
    );

    private static final Map<Character, Integer> autocompleteTable = Map.of(
            ')', 1,
            ']', 2,
            '}', 3,
            '>',4
    );

    private static char isCorrupted(String line) {
        Stack<Character> stack = new Stack<>();
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (pairs.containsKey(c)) stack.push(c);
            else if (stack.isEmpty() || pairs.get(stack.pop()) != c) return c;
        }
        return 'X';
    }

    private static int findSyntaxErrors(List<String> lines) {
        int total = 0;
        for (String line : lines) {
            char c = isCorrupted(line);
            if (c != 'X') total += errorTable.get(c);
        }
        return total;
    }

    private static long completeLine(String line) {
        long total = 0;
        Stack<Character> stack = new Stack<>();
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (pairs.containsKey(c)) stack.push(c);
            else stack.pop();
        }
        while (!stack.isEmpty()) {
            total *= 5;
            total += autocompleteTable.get(pairs.get(stack.pop()));
        }
        return total;
    }

    private static long autocomplete(List<String> lines) {
        List<Long> scores = new ArrayList<>();
        for (String line : lines) {
            if (isCorrupted(line) == 'X')
                scores.add(completeLine(line));
        }
        Collections.sort(scores);
        return scores.get(scores.size() / 2);
    }

    public static void main(String[] args) {
        // load data
        List<String> lines = Input.collectDataToList(
                Input.readData(Year.AOC_2021, "day10.txt"), DataType.LINE
        );

        // Part 1
        System.out.println(findSyntaxErrors(lines));

        // Part 2
        System.out.println(autocomplete(lines));
    }

}
