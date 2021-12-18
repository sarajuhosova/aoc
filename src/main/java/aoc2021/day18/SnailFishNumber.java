package aoc2021.day18;

import library.tuple.Pair;

import java.util.List;

public abstract class SnailFishNumber {

    public abstract long getMagnitude();

    public abstract List<SnailFishNumber> toList();

    public abstract SnailFishNumber findExplode(int depth);

    public abstract void replace(SnailFishNumber toReplace);

    public abstract Pair<Boolean, SnailFishNumber> split();

    public abstract SnailFishNumber copy();

    public static SnailFishNumber read(String number) {
        if (number.charAt(0) == '[') return PairNumber.read(number);
        return RegularNumber.read(number);
    }

}
