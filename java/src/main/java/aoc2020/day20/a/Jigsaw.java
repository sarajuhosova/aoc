package aoc2020.day20.a;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.*;

public class Jigsaw {

    public static List<Tile> parse(Scanner sc) {
        List<Tile> tiles = new ArrayList<>();

        while (sc.hasNextLine()) {
            String[] split = sc.nextLine().split(" ");
            Tile tile = new Tile(Integer.parseInt(split[1].substring(0, split[1].length() - 1)));

            List<String> image = new ArrayList<>();
            while (sc.hasNextLine()) {
                String line = sc.nextLine();
                if (line.equals("")) break;
                image.add(line);
            }
            tile.addImage(image);

            tiles.add(tile);
        }

        return tiles;
    }

    private static List<Pair<Edge, Edge>> getEdgePairs(List<Tile> tiles) {
        List<Pair<Edge, Edge>> pairs = new ArrayList<>();

        List<Edge> edges = new ArrayList<>();
        for (Tile tile : tiles) {
            List<Edge> tileEdges = tile.getEdges();
            for (Edge edge : tileEdges) {
                int index = edges.indexOf(edge);
                if (index == -1) {
                    edges.add(edge);
                } else {
                    pairs.add(new Pair<>(edge, edges.get(index)));
                }
            }
        }

        return pairs;
    }

    public static long calculateEdges(List<Pair<Edge, Edge>> pairs, List<Tile> tiles) {
        Map<Long, Integer> counts = new HashMap<>();
        for (Tile t : tiles) {
            counts.put(t.getId(), 0);
        }

        for (Pair<Edge, Edge> pair : pairs) {
            int first = counts.get(pair.getFirst().getTileId());
            counts.put(pair.getFirst().getTileId(), first + 1);

            int second = counts.get(pair.getSecond().getTileId());
            counts.put(pair.getSecond().getTileId(), second + 1);
        }

        return counts.keySet().stream()
                .filter(k -> counts.get(k) == 2)
                .reduce(1L, (a, b) -> a * b);
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year._2020, "day20.txt");
        List<Tile> tiles = parse(sc);
        sc.close();

        List<Pair<Edge, Edge>> pairs = getEdgePairs(tiles);
        System.out.println(calculateEdges(pairs, tiles));
    }

}
