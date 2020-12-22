package aoc2020.day20.trash;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class JigsawTile {

    private long id;
    private char[][] image;
    private Map<String, JigsawTile> neighbours;
    private Map<Orientation, String> orientation;

    public JigsawTile(Tile tile) {
        this.id = tile.getId();
        this.orientation = tile.getOrientation();

        image = new char[tile.getImage().length - 2][tile.getImage()[0].length - 2];
        for (int i = 1; i < tile.getImage().length - 1; i++) {
            for (int j = 1; j < tile.getImage()[0].length - 1; j++) {
                image[i - 1][j - 1] = tile.getImage()[i][j];
            }
        }

        this.neighbours = new HashMap<>();
    }

    public void setImage(char[][] image) {
        this.image = image;
    }

    public long getId() {
        return id;
    }

    public char[][] getImage() {
        return image;
    }

    public Map<Orientation, String> getOrientation() {
        return orientation;
    }

    private boolean isString(String expected, String actual) {
        return expected.equals(actual) || expected.equals(new StringBuilder(actual).reverse().toString());
    }

    private boolean containsString(Set<String> set, String string) {
        return set.stream().anyMatch(s -> isString(s, string));
    }

    public Map<Orientation, String> getSpecificOrientations(Set<String> strings) {
        return orientation.keySet().stream()
                .filter(k -> containsString(strings, orientation.get(k)))
                .collect(Collectors.toMap(k -> k, k -> orientation.get(k)));
    }

    public Map<String, JigsawTile> getNeighbours() {
        return neighbours;
    }

    public void addNeighbour(JigsawTile node, String connector) {
        neighbours.put(connector, node);
        node.neighbours.put(connector, this);
    }
}
