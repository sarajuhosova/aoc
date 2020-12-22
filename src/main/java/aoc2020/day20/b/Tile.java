package aoc2020.day20.b;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static aoc2020.day20.b.Orientation.*;

public class Tile {
    private long id;
    char[][] image;
    private Map<Orientation, String> edges;
    private Map<Orientation, Tile> neighbours;

    public Tile(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public char[][] getImage() {
        return image;
    }

    public void setImage(char[][] image) {
        this.image = image;
    }

    public Map<Orientation, String> getEdges() {
        return edges;
    }

    public Map<Orientation, Tile> getNeighbours() {
        return neighbours;
    }

    private boolean isEdge(Orientation o, String s) {
        return edges.get(o).equals(s) || edges.get(o).equals(Util.reverse(s));
    }

    public Orientation getOrientation(String s) {
        return edges.keySet().stream()
                .filter(k -> isEdge(k, s))
                .findAny().get();
    }

    public boolean hasEdge(String s) {
        return edges.containsValue(s) || edges.containsValue(Util.reverse(s));
    }

    public void rotateLeft() {
        image = Util.rotateLeft(image);

        // rotate edges
        String top = edges.get(TOP);
        edges.put(TOP, edges.get(RIGHT));
        edges.put(RIGHT, edges.get(BOTTOM));
        edges.put(BOTTOM, edges.get(LEFT));
        edges.put(LEFT, top);

        // flip strings
        edges.put(LEFT, Util.reverse(edges.get(LEFT)));
        edges.put(RIGHT, Util.reverse(edges.get(RIGHT)));

        // rotate neighbours
        Tile topT = neighbours.get(TOP);
        neighbours.put(TOP, neighbours.get(RIGHT));
        neighbours.put(RIGHT, neighbours.get(BOTTOM));
        neighbours.put(BOTTOM, neighbours.get(LEFT));
        neighbours.put(LEFT, topT);
    }

    public void rotateRight() {
        image = Util.rotateRight(image);

        // rotate edges
        String top = edges.get(TOP);
        edges.put(TOP, edges.get(LEFT));
        edges.put(LEFT, edges.get(BOTTOM));
        edges.put(BOTTOM, edges.get(RIGHT));
        edges.put(RIGHT, top);

        // flip strings
        edges.put(TOP, Util.reverse(edges.get(TOP)));
        edges.put(BOTTOM, Util.reverse(edges.get(BOTTOM)));

        // rotate neighbours
        Tile topT = neighbours.get(TOP);
        neighbours.put(TOP, neighbours.get(LEFT));
        neighbours.put(LEFT, neighbours.get(BOTTOM));
        neighbours.put(BOTTOM, neighbours.get(RIGHT));
        neighbours.put(RIGHT, topT);
    }

    public void flipHor() {
        image = Util.flipHor(image);

        // flip strings
        edges.put(TOP, Util.reverse(edges.get(TOP)));
        edges.put(BOTTOM, Util.reverse(edges.get(BOTTOM)));

        // flip edges
        String left = edges.get(LEFT);
        edges.put(LEFT, edges.get(RIGHT));
        edges.put(RIGHT, left);

        // flip neighbours
        Tile leftT = neighbours.get(LEFT);
        neighbours.put(LEFT, neighbours.get(RIGHT));
        neighbours.put(RIGHT, leftT);
    }

    public void flipVert() {
        image = Util.flipVert(image);

        // flip strings
        edges.put(LEFT, Util.reverse(edges.get(LEFT)));
        edges.put(RIGHT, Util.reverse(edges.get(RIGHT)));

        // flip edges
        String top = edges.get(TOP);
        edges.put(TOP, edges.get(BOTTOM));
        edges.put(BOTTOM, top);

        // flip neighbours
        Tile topT = neighbours.get(TOP);
        neighbours.put(TOP, neighbours.get(BOTTOM));
        neighbours.put(BOTTOM, topT);
    }

    public void addNeighbour(Tile tile, Orientation o) {
        neighbours.put(o, tile);
        tile.neighbours.put(tile.getOrientation(edges.get(o)), this);
    }

    public void addImage(List<String> data) {
        assert data != null;
        assert !data.isEmpty();

        image = new char[data.size() - 2][data.get(0).length() - 2];
        for (int i = 1; i < data.size() - 1; i++) {
            String s = data.get(i);
            for (int j = 1; j < s.length() - 1; j++) {
                image[i - 1][j - 1] = s.charAt(j);
            }
        }

        calculateEdges(data);
    }

    private void calculateEdges(List<String> data) {
        String top = data.get(0);
        String bottom = data.get(data.size() - 1);

        String left = "";
        for (int i = 0; i < data.size(); i++) {
            left += data.get(i).charAt(0);
        }

        String right = "";
        int max = data.get(0).length() - 1;
        for (int i = 0; i < data.size(); i++) {
            right += data.get(i).charAt(max);
        }

        edges = new HashMap<>();
        edges.put(TOP, top);
        edges.put(BOTTOM, bottom);
        edges.put(LEFT, left);
        edges.put(RIGHT, right);
        neighbours = new HashMap<>();
    }
}
