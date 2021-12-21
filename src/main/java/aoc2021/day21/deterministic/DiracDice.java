package aoc2021.day21.deterministic;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.Scanner;

public class DiracDice {

    private static Pair<Integer, Integer> read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day21.txt");
        return new Pair<>(
                Integer.parseInt(sc.nextLine().substring(28)),
                Integer.parseInt(sc.nextLine().substring(28))
        );
    }

    public static void main(String[] args) {
        // Read
        Pair<Integer, Integer> players = read();

        // Part 1
        DiracGame game = new DiracGame(new DeterministicDie(), players.getFirst(), players.getSecond());
        Pair<Integer, Integer> result = game.simulatePlay();
        System.out.println(result.getFirst() * result.getSecond());
    }

}
