package aoc2021.day12;

import java.util.ArrayList;
import java.util.List;

public class Cave {

    private String name;
    private List<Cave> neighbours;

    public Cave(String name) {
        this.name = name;
        this.neighbours = new ArrayList<>();
    }

    public String getName() {
        return name;
    }

    public void addNeighbour(Cave cave) {
        neighbours.add(cave);
        cave.neighbours.add(this);
    }

    public List<Cave> getNeighbours() {
        return neighbours;
    }

    public boolean isStart() {
        return name.equals("start");
    }

    public boolean isEnd() {
        return name.equals("end");
    }

    public boolean isSmall() {
        return name.charAt(0) >= 'a' && name.charAt(0) <= 'z';
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(name + ":");
        for (Cave cave : neighbours)
            builder.append(" ").append(cave.name);
        return builder.toString();
    }

}
