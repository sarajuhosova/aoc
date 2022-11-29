package aoc2021.day05;

import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class AvoidingVents {

    private static List<Line> read() {
        Scanner sc = Input.openFile(Year._2021, "day05.txt");
        List<Line> lines = new ArrayList<>();
        while (sc.hasNextLine())
            lines.add(Line.read(sc));
        return lines;
    }

    private static int checkWithoutDiagonals(List<Line> lines) {
        VentMap map = new VentMap(lines, false);
        return map.countDangerousSpots();
    }

    private static int checkWithDiagonals(List<Line> lines) {
        VentMap map = new VentMap(lines, true);
        return map.countDangerousSpots();
    }

    public static void main(String[] args) {
        // load data
        List<Line> lines = read();

        // Part 1
        System.out.println(checkWithoutDiagonals(lines));

        // Part 2
        System.out.println(checkWithDiagonals(lines));
    }

}
