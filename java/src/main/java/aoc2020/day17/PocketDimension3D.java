package aoc2020.day17;

import library.Year;
import library.io.Input;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class PocketDimension3D {

    static int minX;
    static int maxX;

    static int minY;
    static int maxY;

    static int minZ;
    static int maxZ;

    static class Cube {
        int x;
        int y;
        int z;

        public Cube(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Cube cube = (Cube) o;

            if (x != cube.x) return false;
            if (y != cube.y) return false;
            return z == cube.z;
        }

        @Override
        public int hashCode() {
            int result = x;
            result = 31 * result + y;
            result = 31 * result + z;
            return result;
        }
    }

    public static Set<Cube> parse(List<String> lines) {
        Set<Cube> original = new HashSet<>();
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            for (int j = 0; j < line.length(); j++) {
                if (line.charAt(j) == '#')
                    original.add(new Cube(i, j, 0));
            }
        }
        return original;
    }

    public static int countActiveNeighbours(int x, int y, int z, Set<Cube> cubes) {
        int count = 0;
        for (int i = x - 1; i <= x + 1; i++) {
            for (int j = y - 1; j <= y + 1; j++) {
                for (int k = z - 1; k <= z + 1; k++) {
                    if (!(i == x && j == y && k == z) && cubes.contains(new Cube(i, j, k))) count++;
                }
            }
        }
        return count;
    }

    public static void updateBounds(int x, int y, int z) {
        minX = Math.min(minX, x);
        maxX = Math.max(maxX, x);

        minY = Math.min(minY, y);
        maxY = Math.max(maxY, y);

        minZ = Math.min(minZ, z);
        maxZ = Math.max(maxZ, z);
    }

    public static int countActive(Set<Cube> original, int iterations) {
        Set<Cube> next = new HashSet<>();

        for (int it = 0; it < iterations; it++) {
            for (int i = minX - 1; i <= maxX + 1; i++) {
                for (int j = minY - 1; j <= maxY + 1; j++) {
                    for (int k = minZ - 1; k <= maxZ + 1; k++) {
                        int count = countActiveNeighbours(i, j, k, original);
                        Cube c = new Cube(i, j, k);
                        if (original.contains(c) && (count == 2 || count == 3)) {
                            next.add(c);
                        } else if (!original.contains(c) && count == 3) {
                            next.add(c);
                        } else continue;
                        updateBounds(i, j, k);
                    }
                }
            }
            original = next;
            next = new HashSet<>();
        }

        return original.size();
    }

    public static void main(String[] args) {
        List<String> lines = Input.readData(Year._2020, "day17.txt").collect(Collectors.toList());
        Set<Cube> original = parse(lines);
        minX = 0;
        maxX = lines.size();
        minY = 0;
        maxY = lines.get(0).length();
        minZ = 0;
        maxZ = 0;

        System.out.println(countActive(original, 6));
    }

}
