package aoc2021.day04;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

public class Bingo {

    private static Pair<List<Board>, List<Integer>> read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day04.txt");

        List<Integer> draws = Arrays.stream(sc.nextLine().split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());

        List<Board> boards = new ArrayList<>();
        while (sc.hasNext()) {
            sc.nextLine();
            boards.add(Board.read(sc, 5));
        }

        return new Pair<>(boards, draws);
    }

    private static Pair<Board, Integer> winner(List<Board> boards, List<Integer> draws) {
        for (Integer num : draws) {
            for (Board board : boards) {
                board.setNumbers(num);
                if (board.won()) {
                    return new Pair<>(board, num);
                }
            }
        }
        return null;
    }

    private static Pair<Board, Integer> loser(List<Board> boards, List<Integer> draws) {
        Pair<Board, Integer> winner = winner(boards, draws);

        boards.remove(winner.getFirst());

        if (boards.size() == 1) {
            return winner(boards, draws);
        }
        return loser(boards, draws);
    }

    private static void printPair(Pair<Board, Integer> pair) {
        System.out.println(pair.getFirst().getScore() * pair.getSecond());
    }

    public static void main(String[] args) {
        // load data
        Pair<List<Board>, List<Integer>> data = read();
        List<Board> boards = data.getFirst();
        List<Integer> draws = data.getSecond();

        // part 1
        printPair(winner(boards, draws));

        // part 2
        printPair(loser(boards, draws));
    }

}
