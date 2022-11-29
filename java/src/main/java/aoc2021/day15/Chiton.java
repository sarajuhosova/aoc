package aoc2021.day15;

import java.util.ArrayList;
import java.util.List;

public class Chiton {

    private List<Edge> edges;

    public Chiton() {
        this.edges = new ArrayList<>();
    }

    public List<Edge> getEdges() {
        return edges;
    }

    public void addEdge(Edge edge) {
        edges.add(edge);
    }

}
