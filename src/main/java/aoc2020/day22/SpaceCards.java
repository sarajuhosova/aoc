package aoc2020.day22;

import library.Year;
import library.io.Input;

import java.util.*;

public class SpaceCards {

    public static QueuePair parse(Scanner sc) {
        Queue<Integer> qa = new LinkedList<>();
        sc.nextLine();
        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            if (line.equals("")) break;
            qa.add(Integer.parseInt(line));
        }

        sc.nextLine();
        Queue<Integer> qb = new LinkedList<>();
        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            qb.add(Integer.parseInt(line));
        }

        return new QueuePair(qa, qb);
    }

    public static void playRound(QueuePair deal) {
        Queue<Integer> qa = deal.getFirst();
        Queue<Integer> qb = deal.getSecond();

        while (!qa.isEmpty() && !qb.isEmpty()) {
            int a = qa.poll();
            int b = qb.poll();

            if (a > b) {
                qa.add(a);
                qa.add(b);
            } else {
                qb.add(b);
                qb.add(a);
            }
        }
    }

    public static long countScore(Queue<Integer> q) {
        long total = 0;
        for (int i = q.size(); i > 0; i--) {
            total += i * q.poll();
        }
        return total;
    }

    public static long determineCombatWinner(QueuePair deal) {
        playRound(deal);

        return countScore(!deal.getFirst().isEmpty() ? deal.getFirst() : deal.getSecond());
    }

    public static void playRecursiveRound(
            QueuePair deal
    ) {
        Set<QueuePair> visited = new HashSet<>();
        visited.add(deepCopy(deal));

        Queue<Integer> qa = deal.getFirst();
        Queue<Integer> qb = deal.getSecond();

        while (!qa.isEmpty() && !qb.isEmpty()) {
            int a = qa.poll();
            int b = qb.poll();

            if (qa.size() >= a && qb.size() >= b) {
                Queue<Integer> copyA = deepCopy(qa, a);
                Queue<Integer> copyB = deepCopy(qb, b);

                QueuePair pair = new QueuePair(copyA, copyB);
                playRecursiveRound(pair);

                if (!copyA.isEmpty()) {
                    qa.add(a);
                    qa.add(b);
                } else {
                    qb.add(b);
                    qb.add(a);
                }
            } else if (a > b) {
                qa.add(a);
                qa.add(b);
            } else {
                qb.add(b);
                qb.add(a);
            }

            if (visited.contains(deal)) {
                deal.getSecond().removeAll(deal.getSecond());
                return;
            }

            visited.add(deepCopy(deal));
        }
    }

    public static long determineRecursiveCombatWinner(QueuePair deal) {
        playRecursiveRound(deal);

        return countScore(!deal.getFirst().isEmpty() ? deal.getFirst() : deal.getSecond());
    }

    public static void main(String[] args) {
        Scanner sc = Input.openFile(Year._2020, "day22.txt");
        QueuePair deal = parse(sc);

        System.out.println(determineCombatWinner(deepCopy(deal)));
        System.out.println(determineRecursiveCombatWinner(deepCopy(deal)));
    }

    private static QueuePair deepCopy(QueuePair deal) {
        return new QueuePair(
                deepCopy(deal.getFirst(), deal.getFirst().size()),
                deepCopy(deal.getSecond(), deal.getSecond().size())
        );
    }

    private static Queue<Integer> deepCopy(Queue<Integer> q, int amount) {
        int size = q.size();
        Queue<Integer> copy = new LinkedList<>();
        for (int i = 0; i < size; i++) {
            int num = q.poll();
            if (i < amount) copy.add(num);
            q.add(num);
        }
        return copy;
    }

}
