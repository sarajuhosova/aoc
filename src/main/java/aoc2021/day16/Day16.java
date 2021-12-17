package aoc2021.day16;

import library.Year;
import library.io.Input;
import library.tuple.Pair;

public class Day16 {

    private static Packet read() {
        Pair<Packet, String> pair =
                Packet.read(Converter.read(Input.openFile(Year.AOC_2021, "day16.txt").nextLine()));
        System.out.println("Leftover string is \"" + pair.getSecond() + "\".");
        return pair.getFirst();
    }

    public static void main(String[] args) {
        // load data
        Packet packet = read();

        // Part 1
        System.out.println(packet.sumVersions());

        // Part 2
        System.out.println(packet.evaluate());
    }

}
