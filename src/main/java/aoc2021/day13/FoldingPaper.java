package aoc2021.day13;

import com.google.common.collect.Sets;
import library.Point;
import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class FoldingPaper {

    private static Pair<Set<Point>, List<Fold>> read() {
        Scanner sc = Input.openFile(Year.AOC_2021, "day13.txt");

        Set<Point> points = new HashSet<>();
        List<Fold> folds = new ArrayList<>();

        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            if (line.isEmpty()) break;
            points.add(new Point(line));
        }
        while (sc.hasNextLine()) {
            folds.add(Fold.read(sc));
        }

        return new Pair<>(points, folds);
    }

    private static Set<Point> foldUp(Set<Point> points, int value) {
        Set<Point> folded = points.stream()
                .filter(p -> p.getX() < value)
                .collect(Collectors.toSet());

        for (Point point : Sets.difference(points, folded)) {
            int inverted = value - (point.getX() - value);
            folded.add(new Point(inverted, point.getY()));
        }

        return folded;
    }

    private static Set<Point> foldLeft(Set<Point> points, int value) {
        Set<Point> folded = points.stream()
                .filter(p -> p.getY() < value)
                .collect(Collectors.toSet());

        for (Point point : Sets.difference(points, folded)) {
            int inverted = value - (point.getY() - value);
            folded.add(new Point(point.getX(), inverted));
        }

        return folded;
    }

    private static Set<Point> fold(Set<Point> points, Fold fold) {
        if (fold.isX()) return foldUp(points, fold.getValue());
        return foldLeft(points, fold.getValue());
    }

    public static void main(String[] args) {
        // load data
        Pair<Set<Point>, List<Fold>> pair = read();
        Set<Point> points = pair.getFirst();
        List<Fold> folds = pair.getSecond();

        for (Fold fold : folds) {
            points = fold(points, fold);
            System.out.println(points.size());
        }
        print(points);
    }

    private static void print(Set<Point> points) {
        int max_x = points.stream().mapToInt(Point::getX).max().getAsInt() + 1;
        int max_y = points.stream().mapToInt(Point::getY).max().getAsInt() + 1;

        boolean[][] paper = new boolean[max_x][max_y];
        for (Point point : points)
            paper[point.getX()][point.getY()] = true;

        for (int i = 0; i < paper.length; i++) {
            for (int j = 0; j < paper[0].length; j++) {
                System.out.print((paper[i][j]) ? "#" : ".");
            }
            System.out.println();
        }
        System.out.println();
    }

}
