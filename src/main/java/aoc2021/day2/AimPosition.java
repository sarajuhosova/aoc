package aoc2021.day2;

public class AimPosition extends Position {

    private int aim;

    public AimPosition() {
        super();
        this.aim = 0;
    }

    public int getAim() {
        return aim;
    }

    public void setAim(int aim) {
        this.aim = aim;
    }

    @Override
    public void update(Instruction instruction) {
        switch (instruction.getDirection()) {
            case FORWARD -> {
                setHorizontal(getHorizontal() + instruction.getValue());
                setDepth(getDepth() + (aim * instruction.getValue()));
            }
            case DOWN -> aim += instruction.getValue();
            case UP -> aim -= instruction.getValue();
        }
    }

}
