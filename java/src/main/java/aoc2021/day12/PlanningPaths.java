package aoc2021.day12;

import library.Year;
import library.io.Input;

import java.util.*;

public class PlanningPaths {

    private static Map<String, Cave> read() {
        Scanner sc = Input.openFile(Year._2021, "day12.txt");

        Map<String, Cave> caves = new HashMap<>();
        while (sc.hasNextLine()) {
            String[] path = sc.nextLine().split("-");
            if (!caves.containsKey(path[0])) caves.put(path[0], new Cave(path[0]));
            if (!caves.containsKey(path[1])) caves.put(path[1], new Cave(path[1]));
            caves.get(path[0]).addNeighbour(caves.get(path[1]));
        }
        return caves;
    }

    private static Set<List<Cave>> findDirectPaths(Cave current, List<Cave> path) {
        if (current.isEnd()) return Set.of(path);
        if (current.isSmall() && path.contains(current)) return null;

        path.add(current);

        Set<List<Cave>> result = new HashSet<>();
        for (Cave neighbour : current.getNeighbours()) {
            List<Cave> newPath = new ArrayList<>(path);
            Set<List<Cave>> child = findDirectPaths(neighbour, newPath);
            if (child != null) result.addAll(child);
        }
        return result;
    }

    private static Set<List<Cave>> findImprovedPaths(Cave current, List<Cave> path, boolean doubled) {
        if (current.isStart() && !path.isEmpty()) return null;
        if (doubled && current.isSmall() && path.contains(current)) return null;
        if (current.isEnd()) return Set.of(path);

        doubled = current.isSmall() && path.contains(current) || doubled;
        path.add(current);

        Set<List<Cave>> result = new HashSet<>();
        for (Cave neighbour : current.getNeighbours()) {
            List<Cave> newPath = new ArrayList<>(path);
            Set<List<Cave>> child = findImprovedPaths(neighbour, newPath, doubled);
            if (child != null) result.addAll(child);
        }
        return result;
    }

    private static int countPaths(Set<List<Cave>> paths) {
        if (paths == null) return 0;
        return paths.size();
    }

    public static void main(String[] args) {
        // load data
        Map<String, Cave> caves = read();

        // part 1
        System.out.println(countPaths(findDirectPaths(caves.get("start"), new ArrayList<>())));

        // part 2
        System.out.println(countPaths(findImprovedPaths(caves.get("start"), new ArrayList<>(), false)));
    }

}
