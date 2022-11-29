package aoc2021.day17;

import library.Point;
import library.Year;
import library.io.Input;
import library.tuple.Pair;

public class FiringProbes {

    private static Pair<Point, Point> read() {
        String data = Input.openFile(Year._2021, "day17.txt").nextLine().substring(15);

        int index = data.indexOf('.');
        int x_s = Integer.parseInt(data.substring(0, index));
        data = data.substring(index + 2);

        index = data.indexOf(',');
        int x_e = Integer.parseInt(data.substring(0, index));
        data = data.substring(index + 4);

        index = data.indexOf('.');
        int y_s = Integer.parseInt(data.substring(0, index));
        data = data.substring(index + 2);

        int y_e = Integer.parseInt(data);

        return new Pair<>(new Point(x_s, y_s), new Point(x_e, y_e));
    }

    private static void step(Point position, Point velocity) {
        position.translate(velocity);
        int x = velocity.getX();
        if (x != 0) velocity.setX(x + ((x < 0) ? 1 : -1));
        velocity.setY(velocity.getY() - 1);
    }

    private static boolean hit(Pair<Point, Point> target, Point position) {
        return target.getFirst().getX() <= position.getX()
                && position.getX() <= target.getSecond().getX()
                && target.getFirst().getY() <= position.getY()
                && position.getY() <= target.getSecond().getY();
    }

    private static boolean hits(Pair<Point, Point> target, Point velocity) {
        Point position = new Point(0, 0);
        while (position.getX() < target.getSecond().getX() && position.getY() > target.getFirst().getY()) {
            step(position, velocity);
            if (hit(target, position)) return true;
        }
        return false;
    }

    private static Point getHighestStart(Pair<Point, Point> target) {
        for (int j = target.getSecond().getX() * 100; j > 0; j--) {
            for (int i = 0; i <= target.getSecond().getX(); i++) {
                if (hits(target, new Point(i, j))) return new Point(i, j);
            }
        }
        return null;
    }

    private static int getHighestPosition(Point velocity) {
        Point position = new Point(0, 0);
        while (velocity.getY() >= 0) {
            step(position, velocity);
        }
        return position.getY();
    }

    private static int count(Pair<Point, Point> target, int max_y) {
        int total = 0;
        for (int j = target.getFirst().getY(); j <= max_y; j++) {
            for (int i = 0; i <= target.getSecond().getX(); i++) {
                if (hits(target, new Point(i, j)))
                    total++;
            }
        }
        return total;
    }

    public static void main(String[] args) {
        // Read data
        Pair<Point, Point> target = read();

        // Part 1
        Point velocity = getHighestStart(target);
        System.out.println(velocity);
        if (velocity != null) System.out.println(getHighestPosition(velocity.copy()));

        // Part2
        System.out.println(count(target, velocity.getY()));
    }

}
