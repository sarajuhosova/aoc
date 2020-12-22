package aoc2020.day20.a;

public class Edge {

    private long tileId;
    private String shape;

    public Edge(long tileId, String shape) {
        this.tileId = tileId;
        this.shape = shape;
    }

    public long getTileId() {
        return tileId;
    }

    public String getShape() {
        return shape;
    }

    private String reverse(String s) {
        return new StringBuilder(s).reverse().toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Edge edge = (Edge) o;

        return shape != null ? (shape.equals(edge.shape) || shape.equals(reverse(edge.shape))) : edge.shape == null;
    }

    @Override
    public int hashCode() {
        int result = (int) (tileId ^ (tileId >>> 32));
        result = 31 * result + (shape != null ? shape.hashCode() : 0);
        return result;
    }
}