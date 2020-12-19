package aoc2020.day19;

import library.Year;
import library.io.Input;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LanguageValidity {

    static class Rule implements Comparable<Rule> {
        int number;
        Set<String> values;

        public Rule(int number) {
            this.number = number;
            this.values = new HashSet<>();
        }

        public void addValue(String s) {
            values.add(s);
        }

        @Override
        public int compareTo(Rule o) {
            return Integer.compare(this.number, o.number);
        }
    }

    static List<String> messages;
    static List<Rule> rules;
    static int counter;

    public static void parse(Scanner sc) {
        rules = new ArrayList<>();
        while (sc.hasNextLine()) {
            String line = sc.nextLine();
            if (line.equals("")) break;

            String[] data = line.split(": ");
            Rule r = new Rule(Integer.parseInt(data[0]));

            Arrays.stream(data[1].split(" [|] ")).forEach(r::addValue);
            rules.add(r);
        }
        Collections.sort(rules);

        messages = new ArrayList<>();
        while (sc.hasNextLine()) {
            messages.add(sc.nextLine());
        }
    }

    public static String getValidRecursion(int r) {
        Rule rule = rules.get(r);
        String regex = "";
        int i = 0;
        for (String subrule : rule.values) {
            String subRegex = "";
            if (subrule.contains("\"")) subRegex += subrule.substring(1, subrule.length() - 1);
            else {
                String[] data = subrule.split(" ");

                subRegex += getValidRecursion(Integer.parseInt(data[0]));
                if (data.length > 1) {
                    int second = Integer.parseInt(data[1]);
                    if (second == rule.number) {
                        if (data.length == 2) subRegex = "(" + subRegex + "){2,}";
                        else {
                            counter++;
                            subRegex = "(" + subRegex + ")+" + counter + "(" + getValidRecursion(Integer.parseInt(data[2])) + ")+" + counter;
                        }
                    } else {
                        subRegex += getValidRecursion(second);
                        if (data.length == 3) subRegex += getValidRecursion(Integer.parseInt(data[2]));
                    }
                }
            }
            regex += subRegex;
            if (i < rule.values.size() - 1) regex += "|";
            i++;
        }

        return (regex.length() == 1) ? regex : "(" + regex + ")";
    }

    public static long getValidStringsRecursion(int r) {
        counter = 0;
        String regex = getValidRecursion(r);
        if (counter == 0) return messages.stream().filter(m -> m.matches(regex)).count();

        List<Integer> amounts = Stream.iterate(2, a -> a + 1).limit(100).collect(Collectors.toList());

        Set<String> patterns = new HashSet<>();
        for (int i = 1; i <= counter; i++) {
            for (int amount : amounts) {
                patterns.add(regex.replace("+" + 1, "{" + amount + "}"));
            }
        }

        return messages.stream()
                .filter(m -> patterns.stream().anyMatch(m::matches))
                .count();
    }

    public static void main(String[] args) {
//        Scanner sc = Input.openFile(Year.AOC_2020, "day19.txt");
//        parse(sc);
//        System.out.println(getValidStringsRecursion(0));
//        sc.close();

        Scanner sc2 = Input.openFile(Year.AOC_2020, "day19_changed.txt");
        parse(sc2);
        System.out.println(getValidStringsRecursion(0));
        sc2.close();
    }

}
