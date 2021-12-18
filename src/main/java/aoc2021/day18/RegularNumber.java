package aoc2021.day18;

import library.tuple.Pair;

import java.util.List;

public class RegularNumber extends SnailFishNumber {

    private int value;

    public RegularNumber(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public void add(int extra) {
        this.value += extra;
    }

    @Override
    public SnailFishNumber findExplode(int depth) {
        return null;
    }

    @Override
    public void replace(SnailFishNumber toReplace) {}

    public Pair<Boolean, SnailFishNumber> split() {
        if (value < 10) return new Pair<>(false, this);
        int left = value / 2;
        return new Pair<>(
                true,
                new PairNumber(new RegularNumber(left), new RegularNumber(value - left))
        );
    }

    @Override
    public SnailFishNumber copy() {
        return new RegularNumber(value);
    }

    @Override
    public long getMagnitude() {
        return value;
    }

    @Override
    public List<SnailFishNumber> toList() {
        return List.of(this);
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RegularNumber that = (RegularNumber) o;

        return value == that.value;
    }

    @Override
    public int hashCode() {
        return value;
    }

    public static RegularNumber read(String number) {
        return new RegularNumber(Integer.parseInt(number));
    }

}
