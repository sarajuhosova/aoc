package aoc2021.day21.deterministic;

import library.tuple.Pair;

public class DiracGame {

    private final Die die;
    private boolean turn;

    private final Player player1;
    private final Player player2;

    public DiracGame(Die die, int position1, int position2) {
        this.die = die;
        this.turn = true;
        this.player1 = new Player(position1 - 1, die.getMaxScore());
        this.player2 = new Player(position2 - 1, die.getMaxScore());
    }

    private void turn(Player player) {
        int roll = 0;
        for (int i = 0; i < 3; i++) {
            roll += die.roll();
        }
        player.move(roll);
    }

    private void turn() {
        if (turn) turn(player1);
        else turn(player2);

        turn = !turn;
    }

    public Pair<Integer, Integer> simulatePlay() {
        int i = 0;
        while (!player1.won() && !player2.won()) {
            turn();
            i++;
        }
        return new Pair<>(i * 3, Math.min(player1.getScore(), player2.getScore()));
    }

}
