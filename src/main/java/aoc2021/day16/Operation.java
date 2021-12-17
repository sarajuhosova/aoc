package aoc2021.day16;

import java.util.function.BiFunction;

public enum Operation {

    SUM(Long::sum, 0),
    PRODUCT((x, y) -> x * y, 1),
    MIN(Long::min, Integer.MAX_VALUE),
    MAX(Long::max, Integer.MIN_VALUE),
    VALUE((x, y) -> x, 0),
    GREATER((x, y) -> (x > y) ? 1L : 0L, 0),
    LESS((x, y) -> (x < y) ? 1L : 0L, 0),
    EQUAL((x, y) -> (x.equals(y)) ? 1L : 0L, 0);

    private BiFunction<Long, Long, Long> operation;
    private long start;

    Operation(BiFunction<Long, Long, Long> operation, long start) {
        this.operation = operation;
        this.start = start;
    }

    public BiFunction<Long, Long, Long> getOperation() {
        return operation;
    }

    public long getStart() {
        return start;
    }

    public static Operation getFromInt(int op) {
        return Operation.values()[op];
    }
    
}
