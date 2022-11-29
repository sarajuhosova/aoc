package aoc2021.day21.quantum;

public class Player {

    private int position;
    private int score;

    public Player(int position) {
        this.position = position - 1;
        this.score = 0;
    }

    private Player(int position, int score) {
        this.position = position;
        this.score = score;
    }

    public void move(int roll) {
        position = (position + roll) % 10;
        score += position + 1;
    }

    public boolean won() {
        return score >= 21;
    }

    public Player copy() {
        return new Player(position, score);
    }

}
