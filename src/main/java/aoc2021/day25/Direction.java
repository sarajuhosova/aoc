package aoc2021.day25;

import java.util.Arrays;
import java.util.stream.Collectors;

public enum Direction {
    EAST('>'),
    SOUTH('v');

    private final char character;

    Direction(char character) {
        this.character = character;
    }

    public char getCharacter() {
        return character;
    }

    public static Direction getFromCharacter(char c) {
        if (c == '.') return null;
        return Arrays.stream(Direction.values())
                .collect(Collectors.toMap(Direction::getCharacter, d -> d))
                .get(c);
    }

}
