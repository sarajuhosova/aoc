package aoc2020.day24;

import java.util.Map;

public enum Direction {
    EAST("e"),
    SOUTHEAST("se"),
    SOUTHWEST("sw"),
    WEST("w"),
    NORTHWEST("nw"),
    NORTHEAST("ne");

    String shortcut;

    Direction(String shortcut) {
        this.shortcut = shortcut;
    }

    private static Map<Direction, Direction> opposite = Map.of(
            EAST, WEST,
            SOUTHEAST, NORTHWEST,
            SOUTHWEST, NORTHEAST,
            WEST, EAST,
            NORTHWEST, SOUTHEAST,
            NORTHEAST, SOUTHWEST
    );

    public Direction getOpposite() {
        return opposite.get(this);
    }
}
