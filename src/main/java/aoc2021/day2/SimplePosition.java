package aoc2021.day2;

public class SimplePosition extends Position {

    @Override
    public void update(Instruction instruction) {
        switch (instruction.getDirection()) {
            case FORWARD -> setHorizontal(getHorizontal() + instruction.getValue());
            case DOWN -> setDepth(getDepth() + instruction.getValue());
            case UP -> setDepth(getDepth() - instruction.getValue());
        }
    }

}
