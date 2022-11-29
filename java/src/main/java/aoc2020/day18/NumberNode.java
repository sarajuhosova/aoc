package aoc2020.day18;

public class NumberNode extends Node {

    private long value;

    public NumberNode(long value) {
        super(null, null);
        this.value = value;
    }

    public long getValue() {
        return value;
    }

    public void setValue(long value) {
        this.value = value;
    }

}
