package aoc2021.day09;

import library.Point;
import library.Year;
import library.io.Input;

import java.util.*;
import java.util.stream.Collectors;

public class FindBasins {

    private static int[][] convertToArray(List<List<Integer>> read) {
        int[][] data = new int[read.size()][read.get(0).size()];
        for (int i = 0; i < read.size(); i++) {
            for (int j = 0; j < read.get(0).size(); j++) {
                data[i][j] = read.get(i).get(j);
            }
        }
        return data;
    }

    private static int[][] read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day09.txt");
        List<List<Integer>> read = new ArrayList<>();
        while (sc.hasNextLine()) {
            read.add(sc.nextLine().chars()
                    .mapToObj(c -> c - '0')
                    .collect(Collectors.toList()));
        }
        return convertToArray(read);
    }

    private static boolean isLocalMinimum(int[][] data, int i, int j) {
        int here = data[i][j];
        if (i > 0 && data[i - 1][j] <= here) return false;
        if (j > 0 && data[i][j - 1] <= here) return false;
        if (i < data.length - 1 && data[i + 1][j] <= here) return false;
        if (j < data[0].length - 1 && data[i][j + 1] <= here) return false;
        return true;
    }

    private static List<Point> getMinima(int[][] data) {
        List<Point> minima = new ArrayList<>();
        for (int i = 0; i < data.length; i++) {
            for (int j = 0; j < data[0].length; j++) {
                if (isLocalMinimum(data, i, j)) {
                    minima.add(new Point(i, j));
                }
            }
        }
        return minima;
    }

    private static long getRiskLevel(int[][] data, List<Point> minima) {
        long sum = 0;
        for (Point loc : minima) {
            sum += 1 + data[loc.getX()][loc.getY()];
        }
        return sum;
    }

    private static void setBasinLocations(int[][] data, Set<Point> points, Point point) {
        int i = point.getX();
        int j = point.getY();
        if (points.contains(point) || data[i][j] == 9) return;
        points.add(point);
        if (i > 0) setBasinLocations(data, points, new Point(i - 1, j));
        if (j > 0) setBasinLocations(data, points, new Point(i, j - 1));
        if (i < data.length - 1) setBasinLocations(data, points, new Point(i + 1, j));
        if (j < data[0].length - 1) setBasinLocations(data, points, new Point(i, j + 1));
    }

    private static int getBasinSize(int[][] data, Point minimum) {
        Set<Point> points = new HashSet<>();
        setBasinLocations(data, points, minimum);
        return points.size();
    }

    private static int findLargestBasins(int[][] data, List<Point> minima) {
        List<Integer> basins = minima.stream()
                .map(m -> getBasinSize(data, m))
                .sorted()
                .collect(Collectors.toList());

        return basins.get(basins.size() - 1)
                * basins.get(basins.size() - 2)
                * basins.get(basins.size() - 3);
    }

    public static void main(String[] args) {
        // load data
        int[][] data = read();
        List<Point> minima = getMinima(data);

        // Part 1
        System.out.println(getRiskLevel(data, minima));

        // Part 2
        System.out.println(findLargestBasins(data, minima));
    }

}
