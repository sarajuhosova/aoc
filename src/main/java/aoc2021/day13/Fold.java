package aoc2021.day13;

import java.util.Scanner;

public class Fold {

    private final boolean x;
    private final int value;

    public Fold(boolean x, int value) {
        this.x = x;
        this.value = value;
    }

    public static Fold read(Scanner sc) {
        String[] data = sc.nextLine().substring(11).split("=");
        return new Fold(data[0].equals("x"), Integer.parseInt(data[1]));
    }

    public boolean isX() {
        return x;
    }

    public int getValue() {
        return value;
    }

}
