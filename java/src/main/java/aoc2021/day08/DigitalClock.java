package aoc2021.day08;

import com.google.common.collect.Sets;
import library.Year;
import library.io.Input;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static aoc2021.day08.Segment.*;

public class DigitalClock {

    private static final List<Character> charList = List.of('a', 'b', 'c', 'd', 'e', 'f', 'g');

    private static Set<Character> toSet(String s) {
        Set<Character> set = new HashSet<>();
        for (int i = 0; i < s.length(); i++) {
            set.add(s.charAt(i));
        }
        return set;
    }

    private static Map<List<Set<Character>>, String[]> read() {
        Scanner sc = Input.openFile(Year._2021, "day08.txt");

        Map<List<Set<Character>>, String[]> result = new HashMap<>();
        while (sc.hasNextLine()) {
            String[] io = sc.nextLine().split(Pattern.quote(" | "));
            result.put(
                    Arrays.stream(io[0].split(" "))
                            .map(DigitalClock::toSet)
                            .collect(Collectors.toList()),
                    io[1].split(" ")
            );
        }
        return result;
    }

    private static boolean isUnique(int length) {
        return length == 2 || length == 3 || length == 4 || length == 7;
    }

    private static int countUnique(Map<List<Set<Character>>, String[]> outputs) {
        int total = 0;
        for (String[] output : outputs.values()) {
            for (String o : output) {
                if (isUnique(o.length()))
                    total++;
            }
        }
        return total;
    }

    private static Set<Character> getDiff(Set<Character> a, Set<Character> b) {
        return Sets.symmetricDifference(a, b);
    }

    private static void setTop(Map<Character, Segment> map, List<Set<Character>> sorted) {
        for (Character diff : getDiff(sorted.get(0), sorted.get(1)))
            map.put(diff, TOP);
    }

    private static void setCounts(Map<Character, Segment> map, List<Set<Character>> sorted) {
        for (Character c : charList) {
            int total = 0;
            for (Set<Character> set : sorted) {
                if (set.contains(c)) total++;
            }
            if (total == 4) map.put(c, BOTTOM_LEFT);
            else if (total == 6) map.put(c, TOP_LEFT);
            else if (total == 9) map.put(c, BOTTOM_RIGHT);
        }
    }

    private static void setLast(Map<Character, Segment> map, Set<Character> seven, Segment segment) {
        for (Character c : seven) {
            if (!map.containsKey(c))
                map.put(c, segment);
        }
    }

    private static Map<Character, Segment> getSegments(List<Set<Character>> analysis) {
        List<Set<Character>> sorted = analysis.stream()
                .sorted(Comparator.comparing(Set::size))
                .collect(Collectors.toList());
        // will be {1, 7, 4, (2, 3, 5), (0, 6, 9), 8}

        Map<Character, Segment> map = new HashMap<>();
        setTop(map, sorted);
        setCounts(map, sorted);
        setLast(map, sorted.get(1), TOP_RIGHT); // 7
        setLast(map, sorted.get(2), MIDDLE); // 4
        setLast(map, sorted.get(9), BOTTOM); // 8

        return map;
    }


    private static Optional<Digit> toDigit(Map<Character, Segment> map, String output) {
        return Digit.getDigit(output.chars()
                .mapToObj(i -> map.get((char) i))
                .collect(Collectors.toList()));
    }


    private static Digit[] toNumber(Map<Character, Segment> map, String[] output) {
        Digit[] digits = new Digit[output.length];
        for (int i = 0; i < output.length; i++) {
            digits[i] = toDigit(map, output[i]).get();
        }
        return digits;
    }

    private static int determineOutput(List<Set<Character>> analysis, String[] output) {
        return Digit.digitToInt(toNumber(getSegments(analysis), output));
    }

    private static int getDigits(Map<List<Set<Character>>, String[]> outputs) {
        return outputs.keySet().stream()
                .mapToInt(k -> determineOutput(k, outputs.get(k)))
                .sum();
    }

    public static void main(String[] args) {
        // load data
        Map<List<Set<Character>>, String[]> data = read();

        // part 1
        System.out.println(countUnique(data));

        // part2
        System.out.println(getDigits(data));
    }

}
