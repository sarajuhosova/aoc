package aoc2020.day20.a;

import java.util.List;
import java.util.stream.Collectors;

public class Tile {

    private long id;
    char[][] image;
    private List<String> edges;

    public Tile(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public List<Edge> getEdges() {
        return edges.stream()
                .map(e -> new Edge(id, e))
                .collect(Collectors.toList());
    }

    public void addImage(List<String> data) {
        assert data != null;
        assert !data.isEmpty();

        image = new char[data.size()][data.get(0).length()];
        for (int i = 0; i < data.size(); i++) {
            String s = data.get(i);
            for (int j = 0; j < s.length(); j++) {
                image[i][j] = s.charAt(j);
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

        edges = List.of(top, bottom, left, right);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder("Tile: " + id + "\n");

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                builder.append(image[i][j]);
            }
            builder.append("\n");
        }

        return builder.toString();
    }
}
