package aoc2021.day11;

import library.Year;
import library.io.Input;

import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;

public class FlashingOctopi {

    private static int[][] read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day11.txt");
        int[][] map = new int[10][10];
        for (int i = 0; i < 10; i++) {
            String line = sc.nextLine();
            for (int j = 0; j < 10; j++) {
                map[i][j] = line.charAt(j) - '0';
            }
        }
        return map;
    }

    private static Octopus[][] createOctopusMap(int[][] map) {
        Octopus[][] octopi = new Octopus[map.length][map[0].length];
        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[0].length; j++) {
                octopi[i][j] = new Octopus(map[i][j]);
            }
        }
        return octopi;
    }

    private static Set<Octopus> toOctopi(int[][] map) {
        Octopus[][] oMap = createOctopusMap(map);
        Set<Octopus> octopi = new HashSet<>();
        for (int i = 0; i < oMap.length; i++) {
            for (int j = 0; j < oMap[0].length; j++) {
                Octopus octopus = oMap[i][j];
                octopus.setNeighbours(oMap, i, j);
                octopi.add(octopus);
            }
        }
        return octopi;
    }

    private static Set<Octopus> step(Set<Octopus> octopi) {
        // increase all by one
        increaseAll(octopi);

        // flash and update neighbours
        Set<Octopus> flashed = new HashSet<>();
        int size = -1;
        while (flashed.size() != size) {
            size = flashed.size();
            for (Octopus octopus : octopi) {
                if (octopus.flashes() && !flashed.contains(octopus)) {
                    flashed.add(octopus);
                    octopus.increaseAll();
                }
            }
        }

        // reset all flashed octopi
        resetAll(flashed);
        return flashed;
    }

    private static void increaseAll(Set<Octopus> octopi) {
        for (Octopus octopus : octopi) {
            octopus.increasePower();
        }
    }

    private static void resetAll(Set<Octopus> octopi) {
        for (Octopus octopus : octopi) {
            octopus.reset();
        }
    }

    private static long countFlashesIn100Steps(Set<Octopus> octopi) {
        long total = 0;
        for (int i = 0; i < 100; i++) {
            total += step(octopi).size();
        }
        return total;
    }

    private static int getSynchronisationStep(Set<Octopus> octopi) {
        int i = 0;
        Set<Octopus> flashed;
        do {
            flashed = step(octopi);
            i++;
        } while (octopi.size() != flashed.size());
        return i;
    }

    public static void main(String[] args) {
        // load data
        int[][] octopi = read();

        // Part 1
        System.out.println(countFlashesIn100Steps(toOctopi(octopi)));

        // Part 2
        System.out.println(getSynchronisationStep(toOctopi(octopi)));
    }

}
