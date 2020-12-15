package aoc2020.day14;

import library.Year;
import library.io.Input;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class MemoryMasking {

    static class InstructionSet {
        String mask;
        int amountX;
        List<Write> writes;

        public InstructionSet(String mask) {
            this.mask = mask;
            this.writes = new ArrayList<>();
        }

        public InstructionSet(String mask, int amountX) {
            this.mask = mask;
            this.amountX = amountX;
            this.writes = new ArrayList<>();
        }

        public void addWrite(Write write) {
            writes.add(write);
        }
    }

    static class Write {
        int memory;
        int value;

        public Write(int memory, int value) {
            this.memory = memory;
            this.value = value;
        }
    }

    private static String replace(String mask, int n) {
        String num = Integer.toBinaryString(n);

        StringBuilder numBuilder = new StringBuilder(num);
        for (int i = 0; i < mask.length() - num.length(); i++) {
            numBuilder.insert(0, "0");
        }
        num = numBuilder.toString();

        StringBuilder string = new StringBuilder();
        for (int i = 0; i < mask.length(); i++) {
            switch (mask.charAt(i)) {
                case '1':
                    string.append(1);
                    break;
                case '0':
                    string.append(0);
                    break;
                default:
                    string.append(num.charAt(i));
            }
        }

        return string.toString();
    }

    private static long valueAdjusting(List<InstructionSet> instructions) {
        Map<Integer, Long> memory = new HashMap<>();

        for (InstructionSet ins : instructions) {
            String mask = ins.mask;
            for (Write w : ins.writes) {
                memory.put(w.memory, Long.parseLong(replace(mask, w.value), 2));
            }
        }

        return memory.values().stream()
                .reduce(0L, Long::sum);
    }

    private static String replace(String mask, int n, String floating) {
        String num = Integer.toBinaryString(n);

        StringBuilder numBuilder = new StringBuilder(num);
        for (int i = 0; i < mask.length() - num.length(); i++) {
            numBuilder.insert(0, "0");
        }
        num = numBuilder.toString();

        int f = 0;
        StringBuilder string = new StringBuilder();
        for (int i = 0; i < mask.length(); i++) {
            switch (mask.charAt(i)) {
                case '1':
                    string.append(1);
                    break;
                case 'X':
                    string.append(floating.charAt(f));
                    f++;
                    break;
                case '0':
                default:
                    string.append(num.charAt(i));
            }
        }

        return string.toString();
    }

    private static long memoryAdjusting(List<InstructionSet> instructions) {
        Map<Long, Long> memory = new HashMap<>();

        for (InstructionSet ins : instructions) {
            String mask = ins.mask;
            int floats = 1 << ins.amountX;
            String[] stuff = new String[floats];

            for (int i = 0; i < floats; i++) {
                String s = Integer.toBinaryString(i);
                StringBuilder builder = new StringBuilder(s);
                for (int k = 0; k < ins.amountX - s.length(); k++) {
                    builder.insert(0, "0");
                }
                stuff[i] = builder.toString();
            }

            for (Write w : ins.writes) {
                for (int i = 0; i < floats; i++) {
                    memory.put(Long.parseLong(replace(mask, w.memory, stuff[i]), 2), (long) w.value);
                }
            }
        }

        return memory.values().stream()
                .reduce(0L, Long::sum);
    }

    public static List<InstructionSet> read(List<String> lines) {
        List<InstructionSet> instructions = new ArrayList<>();
        int i = -1;
        for (String line : lines) {
            switch (line.charAt(1)) {
                case 'a':
                    String mask = line.substring(7);
                    instructions.add(new InstructionSet(mask, (int) mask.chars().filter(c -> c == (int) 'X').count()));
                    i++;
                    break;
                case 'e':
                    String[] split = line.split(" = ");
                    instructions.get(i).addWrite(new Write(
                            Integer.parseInt(split[0].substring(4, split[0].length()-1)),
                            Integer.parseInt(split[1])
                    ));
                    break;
                default:
            }
        }
        return instructions;
    }

    public static void main(String[] args) {
        // Read
        List<String> lines = Input.readData(Year.AOC_2020, "day14.txt")
                .collect(Collectors.toList());
        List<InstructionSet> instructions = read(lines);

        // Part 1
        System.out.println(valueAdjusting(instructions));

        // Part 2
        System.out.println(memoryAdjusting(instructions));
    }

}
