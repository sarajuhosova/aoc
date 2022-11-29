package aoc2020.day20.b;

import static aoc2020.day20.b.Orientation.*;

import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class Jigsaw {

    static List<Tile> tiles;

    public static void parse(Scanner sc) {
        tiles = new ArrayList<>();

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
    }

    private static void findNeighbours() {
        for (Tile tile : tiles) {
            for (Orientation o : tile.getEdges().keySet()) {
                if (tile.getNeighbours().keySet().contains(o)) continue;
                Optional<Tile> neighbour = tiles.stream()
                        .filter(t -> t != tile && t.hasEdge(tile.getEdges().get(o)))
                        .findAny();
                neighbour.ifPresent(value -> tile.addNeighbour(value, o));
            }
        }
    }

    private static Tile findTopLeftCorner() {
        return tiles.stream()
                .filter(t ->
                        t.getNeighbours().keySet()
                                .stream()
                                .filter(k -> t.getNeighbours().get(k) != null)
                                .allMatch(k -> k == RIGHT || k == BOTTOM)
                ).findAny().get();
    }

    private static void adjust(Tile tile, Orientation o, String s) {
        Orientation current = tile.getOrientation(s);

        // rotation
        if (Orientation.getOpposite(o) == current) {
            tile.rotateRight();
            tile.rotateRight();
        } else if (Orientation.getLeft(o) == current) {
            tile.rotateRight();
        } else if (Orientation.getRight(o) == current) {
            tile.rotateLeft();
        }

        // final adjustion
        if (!s.equals(tile.getEdges().get(o))) {
            switch (o) {
                case TOP, BOTTOM -> tile.flipHor();
                case LEFT, RIGHT -> tile.flipVert();
            }
        }
    }

    private static void adjust(Tile corner) {
        Queue<Tile> q = new LinkedList<>();
        q.add(corner);

        Set<Tile> visited = new HashSet<>();
        visited.add(corner);

        while (!q.isEmpty()) {
            Tile current = q.poll();
            for (Orientation o : current.getNeighbours().keySet()) {
                if (!current.getNeighbours().containsKey(o)) continue;

                Tile neighbour = current.getNeighbours().get(o);
                if (neighbour == null || visited.contains(neighbour)) continue;

                adjust(neighbour, Orientation.getOpposite(o), current.getEdges().get(o));
                visited.add(neighbour);
                q.add(neighbour);
            }
        }
    }

    private static Tile[][] createTileGrid(Tile corner, int downSize, int rightSize) {
        Tile[][] grid = new Tile[downSize][rightSize];

        Tile current = corner;
        for (int i = 0; i < downSize; i++) {
            grid[i][0] = current;
            Tile next = current;
            for (int j = 1; j < rightSize; j++) {
                next = next.getNeighbours().get(RIGHT);
                grid[i][j] = next;
            }
            current = current.getNeighbours().get(BOTTOM);
        }

        return grid;
    }

    private static int calcSize(Tile corner, Orientation o) {
        int size = 1;
        Tile down = corner.getNeighbours().get(o);
        while (down != null) {
            size++;
            down = down.getNeighbours().get(o);
        }
        return size;
    }

    private static char[][] assemble(Tile corner) {
        int downSize = calcSize(corner, BOTTOM);
        int rightSize = calcSize(corner, RIGHT);

        Tile[][] grid = createTileGrid(corner, downSize, rightSize);

        char[][] image = new char[downSize * corner.getImage().length][rightSize * corner.getImage()[0].length];
        int downLength = corner.getImage().length;
        int rightLength = corner.getImage()[0].length;

        for (int i = 0; i < downSize; i++) {
            for (int j = 0; j < rightSize; j++) {
                Tile tile = grid[i][j];
                for (int k = 0; k < downLength; k++) {
                    for (int l = 0; l < rightLength; l++) {
                        image[(downLength * i) + k][(rightLength * j) + l] = tile.getImage()[k][l];
                    }
                }
            }
        }

        return image;
    }

    private static char[][] readMonster() {
        List<String> lines = Input.readData(Year._2020, "day20_monster.txt")
                .collect(Collectors.toList());

        char[][] monster = new char[lines.size()][lines.get(0).length()];
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            for (int j = 0; j < line.length(); j++) {
                monster[i][j] = line.charAt(j);
            }
        }

        return monster;
    }

    private static int countHashtags(char[][] image) {
        int count = 0;

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                if (image[i][j] == '#') count++;
            }
        }

        return count;
    }

    private static int findMonsters(char[][] image, char[][] monster) {
        int count = 0;

        for (int i = 0; i < image.length - monster.length + 1; i++) {
            for (int j = 0; j < image[0].length - monster[0].length + 1; j++) {
                boolean found = true;
                for (int k = 0; k < monster.length; k++) {
                    for (int l = 0; l < monster[0].length; l++) {
                        if (monster[k][l] == '#' && image[i + k][j + l] != '#') found = false;
                    }
                }
                if (found) count++;
            }
        }

        return count;
    }

    private static int countMonsters(char[][] image, char[][] monster) {
        int max = Integer.MIN_VALUE;
        for (int i = 0; i < 8; i++) {
            monster = Util.getNextOrientation(monster, i);
            int amount = findMonsters(image, monster);
            max = Math.max(max, amount);
        }
        return max;
    }

    private static int findWaterRoughness(char[][] image, char[][] monster) {
        return countHashtags(image) - (countMonsters(image, monster) * countHashtags(monster));
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year._2020, "day20.txt");
        parse(sc);
        sc.close();

        findNeighbours();

        Tile corner = findTopLeftCorner();
        adjust(corner);

        char[][] image = assemble(corner);
        Util.printImage(image);

        char[][] monster = readMonster();
        System.out.println(findWaterRoughness(image, monster));

        System.out.println("Ha!");
    }

}
