package aoc2021.day05;

import java.util.Scanner;

public class Line {

    private final Point start;
    private final Point end;

    public Line(Point start, Point end) {
        this.start = start;
        this.end = end;
    }

    public Point getStart() {
        return start;
    }

    public Point getEnd() {
        return end;
    }

    public static Line read(Scanner sc) {
        String[] data = sc.nextLine().split(" -> ");
        return new Line(Point.parse(data[0]), Point.parse(data[1]));
    }

    @Override
    public String toString() {
        return start.toString() + " -> " + end.toString();
    }

}
