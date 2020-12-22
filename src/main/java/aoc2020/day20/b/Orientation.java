package aoc2020.day20.b;

import java.util.Map;

public enum Orientation {
    TOP,
    BOTTOM,
    LEFT,
    RIGHT;

    static Map<Orientation, Orientation> opposite = Map.of(
            TOP, BOTTOM,
            BOTTOM, TOP,
            LEFT, RIGHT,
            RIGHT, LEFT
    );

    public static Orientation getOpposite(Orientation o) {
        return opposite.get(o);
    }

    static Map<Orientation, Orientation> left = Map.of(
            TOP, LEFT,
            LEFT, BOTTOM,
            BOTTOM, RIGHT,
            RIGHT, TOP
    );

    public static Orientation getLeft(Orientation o) {
        return left.get(o);
    }

    static Map<Orientation, Orientation> right = Map.of(
            TOP, RIGHT,
            RIGHT, BOTTOM,
            BOTTOM, LEFT,
            LEFT, TOP
    );

    public static Orientation getRight(Orientation o) {
        return right.get(o);
    }
}
