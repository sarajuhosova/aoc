package aoc2020.day18;

public class OperatorNode extends Node {

    private Operator operator;

    public OperatorNode(Operator operator) {
        super();
        this.operator = operator;
    }

    public Operator getOperator() {
        return operator;
    }

    public void setOperator(Operator operator) {
        this.operator = operator;
    }

}
