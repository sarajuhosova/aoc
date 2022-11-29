package aoc2021.day21.quantum;

import library.tuple.Pair;

public class QuantumDiracGame {

    private static final int[] ROLLS = {3, 4, 5, 6, 7, 8, 9};
    private static final int[] COUNT = {1, 3, 6, 7, 6, 3, 1};

    public static Pair<Long, Long> play(Player player1, Player player2) {
        return play(player1, player2, true);
    }

    public static Pair<Long, Long> play(Player player1, Player player2, boolean turn) {
        if (player1.won()) return new Pair<>(1L, 0L);
        if (player2.won()) return new Pair<>(0L, 1L);

        long p1 = 0L;
        long p2 = 0L;

        for (int i = 0; i < ROLLS.length; i++) {
            Pair<Long, Long> wins = play(
                    next(player1, turn, ROLLS[i]),
                    next(player2, !turn, ROLLS[i]),
                    !turn
            );
            p1 += COUNT[i] * wins.getFirst();
            p2 += COUNT[i] * wins.getSecond();
        }

        return new Pair<>(p1, p2);
    }

    public static Player next(Player player, boolean turn, int roll) {
        Player copy = player.copy();
        if (turn) copy.move(roll);
        return copy;
    }

}
