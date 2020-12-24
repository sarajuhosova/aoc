package aoc2020.day24;

import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class LobbyTiling {

    private static List<List<Direction>> read(List<String> data) {
        List<List<Direction>> tiles = new ArrayList<>();

        for (String s : data) {
            List<Direction> current = new ArrayList<>();
            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                switch (c) {
                    case 'e':
                        current.add(Direction.EAST);
                        break;
                    case 'w':
                        current.add(Direction.WEST);
                        break;
                    case 'n':
                        i++;
                        if (s.charAt(i) == 'e') current.add(Direction.NORTHEAST);
                        else if (s.charAt(i) == 'w') current.add(Direction.NORTHWEST);
                        break;
                    case 's':
                        i++;
                        if (s.charAt(i) == 'e') current.add(Direction.SOUTHEAST);
                        else if (s.charAt(i) == 'w') current.add(Direction.SOUTHWEST);
                        break;
                    default:
                }
            }
            tiles.add(current);
        }

        return tiles;
    }

    private static long countFlipped(List<List<Direction>> tiles, List<Hexagon> honeycomb) {
        Hexagon center = honeycomb.get(honeycomb.size() / 2);

        for (List<Direction> directions : tiles) {
            Hexagon next = center;
            for (Direction direction : directions) next = next.getHexagons().get(direction);
            next.flip();
        }

        return honeycomb.stream().filter(Hexagon::isFlipped).count();
    }

    private static long findPatternOnDay(List<Hexagon> honeycomb, int day) {
        Set<Hexagon> black = honeycomb.stream().filter(Hexagon::isFlipped).collect(Collectors.toSet());

        for (int i = 0; i < day; i++) {
            Set<Hexagon> next = new HashSet<>();
            for (Hexagon h : honeycomb) {
                long blackTiles = h.getHexagons().values().stream().filter(black::contains).count();
                if (black.contains(h) && (blackTiles > 0 && blackTiles <= 2)) next.add(h);
                else if (!black.contains(h) && blackTiles == 2) next.add(h);
            }
            black = next;
        }

        return black.size();
    }

    public static void main(String[] args) {
        List<String> data = Input.readData(Year.AOC_2020, "day24.txt").collect(Collectors.toList());
        List<List<Direction>> tiles = read(data);
        List<Hexagon> honeycomb = Hexagon.buildHoneyComb(tiles.stream().mapToInt(List::size).max().getAsInt() * 10);

        System.out.println(countFlipped(tiles, honeycomb));
        System.out.println(findPatternOnDay(honeycomb, 100));
    }

}
