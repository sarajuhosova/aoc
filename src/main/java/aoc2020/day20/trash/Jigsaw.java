package aoc2020.day20.trash;

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

    private static List<EdgePair> getEdgePairs() {
        List<EdgePair> pairs = new ArrayList<>();

        List<Edge> edges = new ArrayList<>();
        for (Tile tile : tiles) {
            List<Edge> tileEdges = tile.getEdges();
            for (Edge edge : tileEdges) {
                int index = edges.indexOf(edge);
                if (index == -1) {
                    edges.add(edge);
                } else {
                    pairs.add(new EdgePair(edge, edges.get(index)));
                }
            }
        }

        return pairs;
    }

    public static long calculateEdges(List<EdgePair> pairs) {
        Map<Long, Integer> counts = new HashMap<>();
        for (Tile t : tiles) {
            counts.put(t.getId(), 0);
        }

        for (EdgePair pair : pairs) {
            int first = counts.get(pair.getFirst().getTileId());
            counts.put(pair.getFirst().getTileId(), first + 1);

            int second = counts.get(pair.getSecond().getTileId());
            counts.put(pair.getSecond().getTileId(), second + 1);
        }

        return counts.keySet().stream()
                .filter(k -> counts.get(k) == 2)
                .reduce(1L, (a, b) -> a * b);
    }

    private static char[][] readSeaMonster() {
        List<String> lines = Input.readData(Year.AOC_2020, "day20_monster.txt")
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

    private static long countHashtags(char[][] image) {
        long count = 0;

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                if (image[i][j] == '#') count++;
            }
        }

        return count;
    }

    private static long findNonSeaMonsters(char[][] image, char[][] monster, long amount) {
        long count = 0;

        for (int i = 0; i < image.length - monster.length + 1; i++) {
            for (int j = 0; j < image[0].length - monster[0].length + 1; j++) {
                long thisCount = 0;
                boolean contains = true;
                for (int k = 0; k < monster.length; k++) {
                    for (int l = 0; l < monster[0].length; l++) {
                        if (image[i + k][j + l] == '#') thisCount++;
                        if (monster[k][l] == '#' && image[i + k][j + l] != '#') contains = false;
                    }
                }
                count += (contains ? (thisCount - amount) : thisCount);
            }
        }

        return count;
    }

    public static long findNonSeaMonsters(char[][] image) {
        long min = Long.MAX_VALUE;

        char[][] monster = readSeaMonster();
        long hashtagAmount = countHashtags(monster);
        for (int i = 0; i < 8; i++) {
            monster = ImageAssembler.getNextOrientation(monster, i);
            min = Math.min(min, findNonSeaMonsters(image, monster, hashtagAmount));
        }

        return min;
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year.AOC_2020, "day20.txt");
        parse(sc);
        sc.close();

        List<EdgePair> pairs = getEdgePairs();
        System.out.println(calculateEdges(pairs));

        test();

//        char[][] image = assembleImage(pairs);
//        System.out.println(findNonSeaMonsters(image));
    }

    private static void test() {
        int result = 8;
        char[][] monster = readSeaMonster();
        printImage(monster);
        printImage(ImageAssembler.flipVert(monster));

//        List<String> lines = Input.readData(Year.AOC_2020, "test.txt")
//                .collect(Collectors.toList());
//
//        char[][] test = new char[lines.size()][lines.get(0).length()];
//        for (int i = 0; i < lines.size(); i++) {
//            String line = lines.get(i);
//            for (int j = 0; j < line.length(); j++) {
//                test[i][j] = line.charAt(j);
//            }
//        }
//        System.out.println(findNonSeaMonsters(test, monster, countHashtags(monster)));
    }

    private static char[][] deepCopy(char[][] image) {
        char[][] copy = new char[image.length][image[0].length];
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                copy[i][j] = image[i][j];
            }
        }
        return copy;
    }

    private static void printImage(char[][] image) {
        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                System.out.print(image[i][j]);
            }
            System.out.println();
        }
    }

}
