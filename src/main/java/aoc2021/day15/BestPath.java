package aoc2021.day15;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class BestPath {

    private static List<List<Integer>> read() {
        Scanner sc = Input.openFile(Year._2021, "day15.txt");

        List<List<Integer>> data = new ArrayList<>();
        while (sc.hasNextLine()) {
            data.add(sc.nextLine().chars()
                    .mapToObj(c -> c - '0')
                    .collect(Collectors.toList()));
        }
        return data;
    }

    private static List<List<Integer>> expand(List<List<Integer>> original) {
        int height = original.size();
        int width = original.get(0).size();

        List<List<Integer>> result = new ArrayList<>(original);
        for (int x = 0; x < 4; x++) {
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    int risk = result.get(i).get(result.get(i).size() - width) + 1;
                    if (risk > 9) risk = 1;
                    result.get(i).add(risk);
                }
            }
        }

        for (int x = 0; x < 4; x++) {
            for (int i = 0; i < height; i++) {
                List<Integer> list = new ArrayList<>();
                for (int j = 0; j < result.get(i).size(); j++) {
                    int risk = result.get(result.size() - height).get(j) + 1;
                    if (risk > 9) risk = 1;
                    list.add(risk);
                }
                result.add(list);
            }
        }

        return result;
    }

    private static void addEdges(List<List<Integer>> data, Chiton[][] chitons, int i, int j) {
        Chiton origin = chitons[i][j];
        if (i > 0) origin.addEdge(new Edge(data.get(i - 1).get(j), chitons[i - 1][j]));
        if (j > 0) origin.addEdge(new Edge(data.get(i).get(j - 1), chitons[i][j - 1]));
        if (i < chitons.length - 1) origin.addEdge(new Edge(data.get(i + 1).get(j), chitons[i + 1][j]));
        if (j < chitons[0].length - 1) origin.addEdge(new Edge(data.get(i).get(j + 1), chitons[i][j + 1]));
    }

    private static Chiton[][] getChitons(List<List<Integer>> data) {
        Chiton[][] chitons = new Chiton[data.size()][data.get(0).size()];
        for (int i = 0; i < chitons.length; i++) {
            for (int j = 0; j < chitons[0].length; j++) {
                chitons[i][j] = new Chiton();
            }
        }
        for (int i = 0; i < chitons.length; i++) {
            for (int j = 0; j < chitons[0].length; j++) {
                addEdges(data, chitons, i, j);
            }
        }
        return chitons;
    }

    private static Map<Chiton, Pair<Integer, Chiton>> dijkstra(Chiton start, Chiton end) {
        Map<Chiton, Pair<Integer, Chiton>> shortest = new HashMap<>();
        shortest.put(start, new Pair<>(0, null));

        PriorityQueue<Pair<Integer, Chiton>> queue = new PriorityQueue<>(Comparator.comparing(Pair::getFirst));
        queue.add(new Pair<>(0, start));
        while (!queue.isEmpty()) {
            Pair<Integer, Chiton> pair = queue.poll();
            int distance = pair.getFirst();
            Chiton chiton = pair.getSecond();

            if (chiton == end) return shortest;

            for (Edge e : chiton.getEdges()) {
                Chiton destination = e.getDestination();
                int risk = distance + e.getRisk();

                if (!(shortest.containsKey(destination) && risk >= shortest.get(destination).getFirst())) {
                    shortest.put(destination, new Pair<>(risk, chiton));
                    queue.add(new Pair<>(risk, destination));
                }
            }
        }

        return shortest;
    }

    private static int getLowestRiskPath(Chiton start, Chiton end) {
        Map<Chiton, Pair<Integer, Chiton>> shortest = dijkstra(start, end);
        return shortest.get(end).getFirst();
    }

    public static void main(String[] args) {
        // load data
        List<List<Integer>> data = read();

        // Part 1
        Chiton[][] chitons = getChitons(data);
        System.out.println(getLowestRiskPath(chitons[0][0], chitons[chitons.length - 1][chitons[0].length - 1]));

        // Part 2
        List<List<Integer>> expanded = expand(data);
        chitons = getChitons(expanded);
        System.out.println(getLowestRiskPath(chitons[0][0], chitons[chitons.length - 1][chitons[0].length - 1]));
    }

}
