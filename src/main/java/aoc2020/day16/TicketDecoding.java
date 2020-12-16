package aoc2020.day16;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class TicketDecoding {

    static List<Field> fields;
    static List<Integer> ticket;
    static List<List<Integer>> nearby;
    static Set<Integer> valid;

    static class Field {
        String name;
        Set<Integer> valid;

        public Field(String name) {
            this.name = name;
            this.valid = new HashSet<>();
        }

        public void addValid(int min, int max) {
            for (int i = min; i <= max; i++) {
                valid.add(i);
            }
        }
    }

    public static void read(Scanner sc) {
        fields = new ArrayList<>();

        while (sc.hasNextLine()) {
            String line = sc.nextLine();

            if (line.length() == 0) break;

            String[] d = line.split(": | or ");
            Field f = new Field(d[0]);
            String[] lower = d[1].split("-");
            f.addValid(Integer.parseInt(lower[0]), Integer.parseInt(lower[1]));
            String[] upper = d[2].split("-");
            f.addValid(Integer.parseInt(upper[0]), Integer.parseInt(upper[1]));

            fields.add(f);
        }

        sc.nextLine();
        ticket = Arrays.stream(sc.nextLine().split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());

        sc.nextLine();
        sc.nextLine();

        nearby = new ArrayList<>();
        while (sc.hasNextLine()) {
            nearby.add(Arrays.stream(sc.nextLine().split(","))
                    .map(Integer::parseInt)
                    .collect(Collectors.toList()));
        }

        valid = fields.stream()
                .flatMap(f -> f.valid.stream())
                .collect(Collectors.toSet());
    }

    public static long part1() {
        long total = 0L;
        for (List<Integer> t : nearby) {
            for (Integer i : t) {
                if (!valid.contains(i)) total += i;
            }
        }
        return total;
    }

    public static long part2() {
        List<List<Integer>> validNearby = nearby.stream()
                .filter(t -> valid.containsAll(t))
                .collect(Collectors.toList());
        validNearby.add(ticket);

        Map<Field, Integer> map = new HashMap<>();

        Queue<Integer> q = new LinkedList<>();
        for (int i = 0; i < ticket.size(); i++) {
            q.add(i);
        }

        while (!q.isEmpty()) {
            int i = q.poll();
            Set<Integer> values = new HashSet<>();
            for (List<Integer> list : validNearby) {
                values.add(list.get(i));
            }

            boolean found = false;
            for (Field f : fields) {
                if (map.containsKey(f)) continue;
                if (f.valid.containsAll(values)) {
                    map.put(f, i);
                    found = true;
                    break;
                }
            }

            if (!found) {
                List<Field> fs = new ArrayList<>(map.keySet());
                Collections.shuffle(fs);
                for (Field f : fs) {
                    if (f.valid.containsAll(values)) {
                        q.add(map.get(f));
                        map.put(f, i);
                        break;
                    }
                }
            }
        }

        List<Pair<Field, Long>> pairs = map.keySet().stream()
                .filter(f -> f.name.contains("departure"))
                .map(k -> new Pair<>(k, (long) ticket.get(map.get(k))))
                .collect(Collectors.toList());

        return pairs.stream()
                .map(Pair::getSecond)
                .reduce(1L, (a, b) -> a * b);
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year.AOC_2020, "day16.txt");
        read(sc);

        System.out.println(part1());
        System.out.println(part2());
    }

}
