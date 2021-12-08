package aoc2021.day08;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static aoc2021.day08.Segment.*;

public enum Digit {
    ZERO(List.of(TOP, TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT, BOTTOM)),
    ONE(List.of(TOP_RIGHT, BOTTOM_RIGHT)),
    TWO(List.of(TOP, TOP_RIGHT, MIDDLE, BOTTOM_LEFT, BOTTOM)),
    THREE(List.of(TOP, TOP_RIGHT, MIDDLE, BOTTOM_RIGHT, BOTTOM)),
    FOUR(List.of(TOP_LEFT, TOP_RIGHT, MIDDLE, BOTTOM_RIGHT)),
    FIVE(List.of(TOP, TOP_LEFT, MIDDLE, BOTTOM_RIGHT, BOTTOM)),
    SIX(List.of(TOP, TOP_LEFT, MIDDLE, BOTTOM_LEFT, BOTTOM_RIGHT, BOTTOM)),
    SEVEN(List.of(TOP, TOP_RIGHT, BOTTOM_RIGHT)),
    EIGHT(List.of(TOP, TOP_LEFT, TOP_RIGHT, MIDDLE, BOTTOM_LEFT, BOTTOM_RIGHT, BOTTOM)),
    NINE(List.of(TOP, TOP_LEFT, TOP_RIGHT, MIDDLE, BOTTOM_RIGHT, BOTTOM));

    List<Segment> segments;

    Digit(List<Segment> segments) {
        this.segments = segments;
    }

    public static Optional<Digit> getDigit(List<Segment> segments) {
        return Arrays.stream(Digit.values())
                .filter(d -> d.segments.size() == segments.size())
                .filter(d -> d.segments.containsAll(segments))
                .findAny();
    }

    public static int digitToInt(Digit[] digit) {
        int result = 0;
        for (int i = 0; i < digit.length; i++) {
            result += digit[digit.length - i - 1].ordinal() * Math.pow(10, i);
        }
        return result;
    }

}
