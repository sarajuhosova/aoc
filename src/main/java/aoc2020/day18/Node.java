package aoc2020.day18;

abstract class Node {

    private Node left;
    private Node right;

    public Node() {
    }

    public Node(Node left, Node right) {
        this.left = left;
        this.right = right;
    }

    public Node getLeft() {
        return left;
    }

    public Node getRight() {
        return right;
    }

    public void setLeft(Node left) {
        this.left = left;
    }

    public void setRight(Node right) {
        this.right = right;
    }

    public boolean hasLeft() {
        return left != null;
    }

    public boolean hasRight() {
        return right != null;
    }

    public static long solveTree(Node node) {
        if (node instanceof NumberNode) return ((NumberNode) node).getValue();
        if (!(node instanceof OperatorNode) || !node.hasLeft() || !node.hasRight()) throw new IllegalArgumentException();
        long a = solveTree(node.left);
        long b = solveTree(node.right);
        return ((OperatorNode) node).getOperator().getFunction().apply(a, b);
    }

}
