package aoc2021.day18;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class SnailFishHomework {

    private static List<SnailFishNumber> read() {
        Scanner sc = Input.openFile(Year._2021, "day18.txt");

        List<SnailFishNumber> numbers = new ArrayList<>();
        while (sc.hasNextLine()) {
            SnailFishNumber number = SnailFishNumber.read(sc.nextLine());
            numbers.add(number);
        }
        return numbers;
    }

    private static Pair<Boolean, SnailFishNumber> explode(SnailFishNumber number) {
        SnailFishNumber result = number.findExplode(0);
        if (result == null) return new Pair<>(false, number);

        PairNumber toExplode = (PairNumber) result;
        List<SnailFishNumber> regulars = number.toList();
        for (int i = 0; i < regulars.size() - 1; i++) {
            if (regulars.get(i + 1) == toExplode.getLeft())
                ((RegularNumber) regulars.get(i)).add(((RegularNumber) toExplode.getLeft()).getValue());
            if (regulars.get(i) == toExplode.getRight()) {
                ((RegularNumber) regulars.get(i + 1)).add(((RegularNumber) toExplode.getRight()).getValue());
                break;
            }
        }
        number.replace(toExplode);
        return new Pair<>(true, number);
    }

    private static SnailFishNumber add(SnailFishNumber left, SnailFishNumber right) {
        SnailFishNumber result = new PairNumber(left, right);
        boolean stop = false;
        while (!stop) {
            Pair<Boolean, SnailFishNumber> pair = explode(result);
            if (!pair.getFirst()) {
                pair = result.split();
                if (pair.getFirst()) result = pair.getSecond();
                else stop = true;
            } else result = pair.getSecond();
        }
        return result;
    }

    private static SnailFishNumber addAll(List<SnailFishNumber> numbers) {
        SnailFishNumber result = numbers.get(0);
        for (int i = 1; i < numbers.size(); i++) {
            result = add(result, numbers.get(i));
        }
        return result;
    }

    private static long findLargestMagnitude(List<SnailFishNumber> numbers) {
        long max = Integer.MIN_VALUE;
        for (int i = 0; i < numbers.size(); i++) {
            for (int j = 0; j < numbers.size(); j++) {
                if (i != j) {
                    long mag = add(numbers.get(i).copy(), numbers.get(j).copy()).getMagnitude();
                    if (mag > max) max = mag;
                }
            }
        }
        return max;
    }

    public static void main(String[] args) {
        // Part 1
        System.out.println(addAll(read()).getMagnitude());

        // Part 2
        System.out.println(findLargestMagnitude(read()));
    }

}
