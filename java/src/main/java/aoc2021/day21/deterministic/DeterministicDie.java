package aoc2021.day21.deterministic;

public class DeterministicDie extends Die {

    private int rolled;

    public DeterministicDie() {
        super(1000);
        this.rolled = 0;
    }

    @Override
    public int roll() {
        if (rolled >= 100) rolled = rolled % 100;
        rolled++;
        return rolled;
    }

}
