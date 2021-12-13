package library;

import java.util.Scanner;

public class Point {

    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Point(String s) {
        String[] data = s.split(",");
        this.x = Integer.parseInt(data[0]);
        this.y = Integer.parseInt(data[1]);
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public static Point read(Scanner sc) {
        String[] data = sc.nextLine().split(",");
        return new Point(Integer.parseInt(data[0]), Integer.parseInt(data[1]));
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Point point = (Point) o;

        if (x != point.x) return false;
        return y == point.y;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }

}