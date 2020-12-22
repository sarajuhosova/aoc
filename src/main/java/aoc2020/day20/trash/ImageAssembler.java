package aoc2020.day20.trash;

import java.util.*;
import java.util.stream.Collectors;

public class ImageAssembler {

    public static char[][] rotate(char[][] image) {
        char[][] rotated = new char[image[0].length][image.length];

        for (int i = 0; i < image[0].length; i++) {
            for (int j = 0; j < image.length; j++) {
                rotated[i][j] = image[j][i];
            }
        }

        return rotated;
    }

    public static char[][] flipVert(char[][] image) {
        char[][] rotated = new char[image.length][image[0].length];

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                rotated[i][j] = image[i][image[0].length - 1 - j];
            }
        }

        return rotated;
    }

    public static char[][] flipHor(char[][] image) {
        char[][] rotated = new char[image.length][image[0].length];

        for (int i = 0; i < image.length; i++) {
            for (int j = 0; j < image[0].length; j++) {
                rotated[i][j] = image[image.length - 1 - i][j];
            }
        }

        return rotated;
    }

    public static char[][] getNextOrientation(char[][] image, int count) {
        return switch (count) {
            case 1, 3, 6 -> flipVert(image);
            case 2, 5, 7 -> flipHor(image);
            case 4 -> rotate(image);
            default -> image;
        };
    }

    private static Set<JigsawTile> assembleImageGraph(List<EdgePair> pairs, List<Tile> tiles) {
        Map<Long, JigsawTile> map = tiles.stream()
                .collect(Collectors.toMap(Tile::getId, JigsawTile::new));

        for (EdgePair pair : pairs) {
            map.get(pair.getFirst().getTileId())
                    .addNeighbour(map.get(pair.getSecond().getTileId()), pair.getSecond().getShape());
        }

        return new HashSet<>(map.values());
    }

    private static String reverse(String string) {
        return new StringBuilder(string).reverse().toString();
    }

    private static void adjust(JigsawTile tile, Orientation o, String s) {
        Orientation t = tile.getOrientation().keySet().stream()
                .filter(oh -> tile.getOrientation().get(oh).equals(s)
                        || tile.getOrientation().get(oh).equals(reverse(s)))
                .findAny().get();

        switch (o) {
            case TOP:
                switch (t) {
                    case BOTTOM:
                        if (!s.equals(tile.getOrientation().get(Orientation.BOTTOM))) tile.setImage(flipVert(tile.getImage()));
                        break;
                    case TOP:
                        tile.setImage(flipHor(tile.getImage()));
                        if (!s.equals(tile.getOrientation().get(Orientation.BOTTOM))) tile.setImage(flipVert(tile.getImage()));
                        break;
                    case RIGHT:
                        break;
                    case LEFT:
                        break;
                }
                break;
            case BOTTOM:
                break;
            case LEFT:
                break;
            case RIGHT:
                break;
        }
    }

    public static char[][] assembleImage(List<EdgePair> pairs, List<Tile> tiles) {
        Set<JigsawTile> imageGraph = assembleImageGraph(pairs, tiles);
        Set<JigsawTile> visited = new HashSet<>();

        Queue<JigsawTile> q = new LinkedList<>();
        JigsawTile current = imageGraph.stream()
                .filter(jt -> jt.getNeighbours().size() == 2)
                .findAny()
                .get();
        visited.add(current);
        q.add(current);

        while (!q.isEmpty()) {
            current = q.poll();
            Map<Orientation, String> orientations = current
                    .getSpecificOrientations(current.getNeighbours().keySet());
            visited.add(current);

            for (Orientation o : orientations.keySet()) {
                JigsawTile child = current.getNeighbours().get(orientations.get(o));
                if (child == null) child = current.getNeighbours().get(reverse(orientations.get(o)));

                if (visited.contains(child)) continue;
                adjust(child, o, orientations.get(o));
                visited.add(child);
                q.add(child);
            }
        }


        return null;
    }
    
}
