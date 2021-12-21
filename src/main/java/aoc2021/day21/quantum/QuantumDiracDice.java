package aoc2021.day21.quantum;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.Scanner;

public class QuantumDiracDice {

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
        Pair<Long, Long> wins = QuantumDiracGame.play(
                new Player(players.getFirst()),
                new Player(players.getSecond())
        );
        System.out.println("Player 1 got " + wins.getFirst() + " wins.");
        System.out.println("Player 2 got " + wins.getSecond() + " wins.");
    }

}
