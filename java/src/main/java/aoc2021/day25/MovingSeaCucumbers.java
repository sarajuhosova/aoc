package aoc2021.day25;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import static aoc2021.day25.Direction.*;

public class MovingSeaCucumbers {

    private static List<List<Direction>> read() {
        Scanner sc = Input.openFile(Year._2021, "day25.txt");

        List<List<Direction>> direction = new ArrayList<>();
        while (sc.hasNextLine()) {
            direction.add(sc.nextLine().chars()
                    .mapToObj(c -> Direction.getFromCharacter((char) c))
                    .collect(Collectors.toList()));
        }
        return direction;
    }

    private static Direction[][] toArray(List<List<Direction>> list) {
        Direction[][] array = new Direction[list.size()][list.get(0).size()];
        for (int i = 0; i < array.length; i++) {
            for (int j = 0; j < array[0].length; j++) {
                array[i][j] = list.get(i).get(j);
            }
        }
        return array;
    }

    private static boolean moveEast(Direction[][] cucumbers, Direction[][] moved) {
        boolean changed = false;
        for (int i = 0; i < cucumbers.length; i++) {
            for (int j = 0; j < cucumbers[i].length; j++) {
                int next = (j + 1) % cucumbers[i].length;
                if (cucumbers[i][j] == EAST) {
                    if (cucumbers[i][next] == null) {
                        moved[i][next] = EAST;
                        changed = true;
                    } else moved[i][j] = EAST;
                }
            }
        }
        return changed;
    }

    private static boolean moveSouth(Direction[][] cucumbers, Direction[][] moved) {
        boolean changed = false;
        for (int i = 0; i < cucumbers.length; i++) {
            int next = (i + 1) % cucumbers.length;
            for (int j = 0; j < cucumbers[i].length; j++) {
                if (cucumbers[i][j] == SOUTH) {
                    if (moved[next][j] == null && cucumbers[next][j] != SOUTH) {
                        moved[next][j] = SOUTH;
                        changed = true;
                    } else moved[i][j] = SOUTH;
                }
            }
        }
        return changed;
    }

    private static Pair<Boolean, Direction[][]> move(Direction[][] cucumbers) {
        Direction[][] moved = new Direction[cucumbers.length][cucumbers[0].length];
        boolean changed = moveEast(cucumbers, moved);
        changed = changed | moveSouth(cucumbers, moved);
        return new Pair<>(changed, moved);
    }

    private static int moves(Direction[][] cucumbers) {
        int i = 0;
        while (true) {
            i++;
            Pair<Boolean, Direction[][]> pair = move(cucumbers);
            if (!pair.getFirst()) break;
            cucumbers = pair.getSecond();
        }
        return i;
    }

    public static void main(String[] args) {
        Direction[][] cucumbers = toArray(read());

        System.out.println(moves(cucumbers));
    }

}
