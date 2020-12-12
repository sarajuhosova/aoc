package aoc2020.day12;

import library.Year;
import library.io.Input;
import library.tuple.Pair;
import library.tuple.Tuple;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Collectors;

public class ShipNavigation {

    enum Action {
        NORTH('N', (v, p) -> new Pair<>(p.getFirst(), p.getSecond() + v)),
        SOUTH('S', (v, p) -> new Pair<>(p.getFirst(), p.getSecond() - v)),
        EAST('E', (v, p) -> new Pair<>(p.getFirst() + v, p.getSecond())),
        WEST('W', (v, p) -> new Pair<>(p.getFirst() - v, p.getSecond())),
        FORWARD('F'),
        RIGHT('R'),
        LEFT('L');

        char symbol;
        BiFunction<Integer, Pair<Integer, Integer>, Pair<Integer, Integer>> move;

        Action(char symbol) {
            this.symbol = symbol;
        }

        Action(char symbol, BiFunction<Integer, Pair<Integer, Integer>, Pair<Integer, Integer>> move) {
            this.symbol = symbol;
            this.move = move;
        }
    }

    static class Instruction {
        Action action;
        int value;

        public Instruction(Action action, int value) {
            this.action = action;
            this.value = value;
        }
    }

    static Map<Character, Action> map = Arrays.stream(Action.values())
            .collect(Collectors.toMap(k -> k.symbol, k -> k));

    static Map<Action, Action> left = Map.of(
            Action.NORTH, Action.WEST,
            Action.WEST, Action.SOUTH,
            Action.SOUTH, Action.EAST,
            Action.EAST, Action.NORTH);

    static Map<Action, Action> right = Map.of(
            Action.NORTH, Action.EAST,
            Action.EAST, Action.SOUTH,
            Action.SOUTH, Action.WEST,
            Action.WEST, Action.NORTH);

    public static Pair<Integer, Integer> changePosition(List<Instruction> instructions) {
        Pair<Integer, Integer> position = Tuple.of(0, 0);
        Action current = Action.EAST;

        for (Instruction i : instructions) {
            switch (i.action) {
                case NORTH:
                case SOUTH:
                case EAST:
                case WEST:
                    position = i.action.move.apply(i.value, position);
                    break;
                case LEFT:
                    for (int j = 0; j < i.value / 90; j++) {
                        current = left.get(current);
                    }
                    break;
                case RIGHT:
                    for (int j = 0; j < i.value / 90; j++) {
                        current = right.get(current);
                    }
                    break;
                case FORWARD:
                    position = current.move.apply(i.value, position);
                    break;
                default:
                    System.out.println("Oops");
            }
        }

        return position;
    }

    public static Pair<Integer, Integer> moveWaypoint(List<Instruction> instructions) {
        Pair<Integer, Integer> position = Tuple.of(0, 0);
        Pair<Integer, Integer> wayPoint = Tuple.of(10, 1);

        for (Instruction i : instructions) {
            switch (i.action) {
                case NORTH:
                case SOUTH:
                case EAST:
                case WEST:
                    wayPoint = i.action.move.apply(i.value, wayPoint);
                    break;
                case LEFT:
                    for (int j = 0; j < i.value / 90; j++) {
                        int s = wayPoint.getSecond();;
                        wayPoint.setSecond(wayPoint.getFirst());
                        wayPoint.setFirst(- 1 * s);
                    }
                    break;
                case RIGHT:
                    for (int j = 0; j < i.value / 90; j++) {
                        int f = wayPoint.getFirst();
                        wayPoint.setFirst(wayPoint.getSecond());
                        wayPoint.setSecond(-1 * f);
                    }
                    break;
                case FORWARD:
                    position.setFirst(position.getFirst() + (i.value * wayPoint.getFirst()));
                    position.setSecond(position.getSecond() + (i.value * wayPoint.getSecond()));
                    break;
                default:
                    System.out.println("Oops");
            }
        }

        return position;
    }

    public static void main(String[] args) {
        List<Instruction> instructions = Input.readData(Year.AOC_2020, "day12.txt")
                .map(s -> new Instruction(map.get(s.charAt(0)), Integer.parseInt(s.substring(1))))
                .collect(Collectors.toList());

        Pair<Integer, Integer> position1 = changePosition(instructions);
        System.out.println(Math.abs(position1.getFirst()) + Math.abs(position1.getSecond()));

        Pair<Integer, Integer> position2 = moveWaypoint(instructions);
        System.out.println(Math.abs(position2.getFirst()) + Math.abs(position2.getSecond()));
    }

}
