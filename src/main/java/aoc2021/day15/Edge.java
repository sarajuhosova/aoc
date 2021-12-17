package aoc2021.day15;

public class Edge implements Comparable<Edge> {

    private int risk;
    private Chiton destination;

    public Edge(int risk, Chiton destination) {
        this.risk = risk;
        this.destination = destination;
    }

    public int getRisk() {
        return risk;
    }

    public Chiton getDestination() {
        return destination;
    }

    @Override
    public int compareTo(Edge o) {
        return Integer.compare(risk, o.risk);
    }

}
