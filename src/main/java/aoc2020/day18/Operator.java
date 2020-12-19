package aoc2020.day18;

import java.util.function.BiFunction;

enum Operator {
    ADDITION('+', Long::sum),
    MULTIPLICATION('*', (a, b) -> a * b);

    char c;
    BiFunction<Long, Long, Long> function;

    Operator(char c, BiFunction<Long, Long, Long> function) {
        this.c = c;
        this.function = function;
    }

    public BiFunction<Long, Long, Long> getFunction() {
        return function;
    }

}