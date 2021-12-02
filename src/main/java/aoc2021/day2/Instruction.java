package aoc2021.day2;

public class Instruction {

    private Direction direction;
    private int value;

    public Instruction(Direction direction, int value) {
        this.direction = direction;
        this.value = value;
    }

    public Direction getDirection() {
        return direction;
    }

    public int getValue() {
        return value;
    }

}
