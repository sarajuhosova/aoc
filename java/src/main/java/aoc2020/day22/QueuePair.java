package aoc2020.day22;

import library.tuple.Pair;

import java.util.Queue;
import java.util.stream.Collectors;

public class QueuePair extends Pair<Queue<Integer>, Queue<Integer>> {

    public QueuePair(Queue<Integer> integers, Queue<Integer> integers2) {
        super(integers, integers2);
    }

    @Override
    public int hashCode() {
        return toString().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        if (!(obj instanceof QueuePair)) return false;

        return this.toString().equals(((QueuePair) obj).toString());
    }

    @Override
    public String toString() {
        String q1 = "Queue 1: " + getFirst().stream().map(Object::toString).collect(Collectors.joining(" "));
        String q2 = "Queue 2: " + getSecond().stream().map(Object::toString).collect(Collectors.joining(" "));
        return q1 + "\n" + q2;
    }
}
