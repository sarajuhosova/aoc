package aoc2021.day2;

import java.util.List;

public abstract class Position {

    private int depth;
    private int horizontal;

    public Position() {
        this.depth = 0;
        this.horizontal = 0;
    }

    public int getDepth() {
        return depth;
    }

    public int getHorizontal() {
        return horizontal;
    }

    public void setDepth(int depth) {
        this.depth = depth;
    }

    public void setHorizontal(int horizontal) {
        this.horizontal = horizontal;
    }

    public abstract void update(Instruction instruction);

    public void update(List<Instruction> instructions) {
        for (Instruction instruction : instructions) {
            this.update(instruction);
        }
    }

}
