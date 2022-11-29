package aoc2021.day20;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

public class EnhancingImages {

    private static Pair<String, List<String>> read() {
        Scanner sc = Input.openFile(Year._2021, "day20.txt");
        String algorithms = sc.nextLine().chars()
                .mapToObj(c -> (c == '#' ? "1" : "0"))
                .collect(Collectors.joining());

        sc.nextLine();

        List<String> image = new ArrayList<>();
        while (sc.hasNextLine()) {
            image.add(sc.nextLine().chars()
                    .mapToObj(c -> (c == '#' ? "1" : "0"))
                    .collect(Collectors.joining()));
        }
        return new Pair<>(algorithms, image);
    }

    private static void pad(List<String> image, String pad) {
        image.add(0, pad.repeat(image.get(0).length() + 2));
        for (int i = 1; i < image.size(); i++) {
            image.set(i, pad + image.get(i) + pad);
        }
        image.add(image.get(0));
    }

    private static String getSubstring(List<String> image, int i, int j) {
        return image.get(i - 1).substring(j - 1, j + 2)
                + image.get(i).substring(j - 1, j + 2)
                + image.get(i + 1).substring(j - 1, j + 2);
    }

    private static int getNum(List<String> image, int i, int j) {
        return Integer.parseInt(getSubstring(image, i, j), 2);
    }

    private static String buildRow(List<String> image, String algorithm, int row) {
        StringBuilder result = new StringBuilder();
        for (int i = 1; i < image.get(row).length() - 1; i++) {
            int index = getNum(image, row, i);
            result.append(algorithm.charAt(index));
        }
        return result.toString();
    }

    private static List<String> enhance(List<String> image, String algorithm) {
        pad(image, image.get(0).charAt(0) + "");
        pad(image, image.get(0).charAt(0) + "");

        List<String> result = new ArrayList<>();
        for (int i = 1; i < image.size() - 1; i++) {
            result.add(buildRow(image, algorithm, i));
        }
        return result;
    }

    private static List<String> enhance(List<String> image, String algorithm, int amount) {
        List<String> result = image;
        for (int i = 0; i < amount; i++)
            result = enhance(result, algorithm);
        return result;
    }

    private static long count(List<String> image) {
        long total = 0;
        for (int i = 0; i < image.size(); i++) {
            for (int j = 0; j < image.get(0).length(); j++) {
                if (image.get(i).charAt(j) == '1') total++;
            }
        }
        return total;
    }

    public static void main(String[] args) {
        Pair<String, List<String>> pair = read();
        String algorithm = pair.getFirst();
        List<String> image = pair.getSecond();
        pad(image, "0");
        pad(image, "0");

        // Part 1
        System.out.println(count(enhance(image, algorithm, 2)));

        // Part 2
        System.out.println(count(enhance(image, algorithm, 50)));
    }

}
