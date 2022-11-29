package aoc2021.day21.deterministic;

public abstract class Die {

    private final int MAX_SCORE;

    public Die(int MAX_SCORE) {
        this.MAX_SCORE = MAX_SCORE;
    }

    public int getMaxScore() {
        return MAX_SCORE;
    }

    public abstract int roll();

}
