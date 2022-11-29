package aoc2021.day11;

import java.util.HashSet;
import java.util.Set;

public class Octopus {

    private int power;
    private Set<Octopus> neighbours;

    public Octopus(int power) {
        this.power = power;
        this.neighbours = new HashSet<>();
    }

    public void setNeighbours(Octopus[][] map, int i, int j) {
        // top
        if (i > 0) neighbours.add(map[i - 1][j]);
        // left
        if (j > 0) neighbours.add(map[i][j - 1]);
        // bottom
        if (i < map.length - 1) neighbours.add(map[i + 1][j]);
        // right
        if (j < map[0].length - 1) neighbours.add(map[i][j + 1]);
        // top left
        if (i > 0 && j > 0) neighbours.add(map[i - 1][j - 1]);
        // top right
        if (i > 0 && j < map[0].length - 1) neighbours.add(map[i - 1][j + 1]);
        // bottom left
        if (i < map.length - 1 && j > 0) neighbours.add(map[i + 1][j - 1]);
        // bottom right
        if (i < map.length - 1 && j < map[0].length - 1) neighbours.add(map[i + 1][j + 1]);
    }

    public boolean flashes() {
        return power > 9;
    }

    public void increasePower() {
        power++;
    }

    public void increaseAll() {
        for (Octopus octopus : neighbours) {
            octopus.increasePower();
        }
    }

    public void reset() {
        power = 0;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("Octopus " + power + ":");
        for (Octopus octopus : neighbours) {
            builder.append(" ").append(octopus.power);
        }
        return builder.toString();
    }
}
