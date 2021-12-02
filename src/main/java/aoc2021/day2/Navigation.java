package aoc2021.day2;

import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Navigation {

    private static List<Instruction> read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day2.txt");
        List<Instruction> data = new ArrayList<>();
        while (sc.hasNextLine()) {
            data.add(new Instruction(
                    Direction.valueOf(sc.next().toUpperCase()),
                    sc.nextInt()
            ));
        }
        return data;
    }

    private static int getFinalPosition(Position start, List<Instruction> instructions) {
        start.update(instructions);
        return start.getDepth() * start.getHorizontal();
    }

    public static void main(String[] args) {
        // load data
        List<Instruction> data = read();

        // Part 1
        System.out.println(getFinalPosition(new SimplePosition(), data));

        // Part 2
        System.out.println(getFinalPosition(new AimPosition(), data));
    }

}
