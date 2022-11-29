package aoc2021.day21.deterministic;

public class Player {

    private int position;
    private int score;
    private final int max;

    public Player(int position, int max) {
        this.position = position;
        this.score = 0;
        this.max = max;
    }

    public int getScore() {
        return score;
    }

    public void move(int roll) {
        position = (position + roll) % 10;
        score += position + 1;
    }

    public boolean won() {
        return score >= max;
    }

}
