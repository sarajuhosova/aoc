package aoc2021.day18;

import library.tuple.Pair;

import java.util.ArrayList;
import java.util.List;

public class PairNumber extends SnailFishNumber {

    private SnailFishNumber left;
    private SnailFishNumber right;

    public PairNumber(SnailFishNumber left, SnailFishNumber right) {
        this.left = left;
        this.right = right;
    }

    public SnailFishNumber getLeft() {
        return left;
    }

    public SnailFishNumber getRight() {
        return right;
    }

    @Override
    public long getMagnitude() {
        return 3 * (left.getMagnitude()) + 2 * (right.getMagnitude());
    }

    @Override
    public List<SnailFishNumber> toList() {
        List<SnailFishNumber> combined = new ArrayList<>();
        combined.addAll(left.toList());
        combined.addAll(right.toList());
        return combined;
    }

    @Override
    public SnailFishNumber findExplode(int depth) {
        if (depth >= 4) return this;
        SnailFishNumber left = this.left.findExplode(depth + 1);
        return (left == null) ? this.right.findExplode(depth + 1) : left;
    }

    @Override
    public void replace(SnailFishNumber toReplace) {
        if (left == toReplace) left = new RegularNumber(0);
        else if (right == toReplace) right = new RegularNumber(0);
        else {
            left.replace(toReplace);
            right.replace(toReplace);
        }
    }

    @Override
    public Pair<Boolean, SnailFishNumber> split() {
        Pair<Boolean, SnailFishNumber> pair = left.split();
        if (pair.getFirst()) {
            left = pair.getSecond();
            return new Pair<>(true,this);
        }
        pair = right.split();
        if (pair.getFirst()) right = pair.getSecond();
        return new Pair<>(pair.getFirst(), this);
    }

    @Override
    public SnailFishNumber copy() {
        return new PairNumber(left.copy(), right.copy());
    }

    @Override
    public String toString() {
        return "[" + left.toString() + "," + right.toString() + "]";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        PairNumber that = (PairNumber) o;

        if (left != null ? !left.equals(that.left) : that.left != null) return false;
        return right != null ? right.equals(that.right) : that.right == null;
    }

    @Override
    public int hashCode() {
        int result = left != null ? left.hashCode() : 0;
        result = 31 * result + (right != null ? right.hashCode() : 0);
        return result;
    }

    public static PairNumber read(String number) {
        number = number.substring(1, number.length() - 1);

        if (number.charAt(0) != '[') {
            int index = number.indexOf(',');
            return new PairNumber(
                    RegularNumber.read(number.substring(0, index)),
                    SnailFishNumber.read(number.substring(index + 1))
            );
        }

        int count = 0;
        int index = 0;
        while (index < number.length()) {
            if (number.charAt(index) == '[') count++;
            if (number.charAt(index) == ']') count--;
            index++;
            if (count == 0) break;
        }

        return new PairNumber(
                SnailFishNumber.read(number.substring(0, index)),
                SnailFishNumber.read(number.substring(index + 1))
        );
    }

}
