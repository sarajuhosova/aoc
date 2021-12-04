package aoc2020.day07;

import java.util.*;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

public class LuggageProcessing {

	static class Bag {
		String colour;
		Map<String, Integer> inside;

		public Bag(String colour) {
			this.colour = colour;
			this.inside = new HashMap<>();
		}

		public void addInside(String colour, int amount) {
			if (inside.containsKey(colour))
				inside.put(colour, inside.get(colour) + amount);
			else
				inside.put(colour, amount);
		}

		public boolean hasBags() {
			return !inside.isEmpty();
		}

		public boolean contains(String colour) {
			return inside.containsKey(colour);
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			if (o == null || getClass() != o.getClass())
				return false;
			return colour.equals(((Bag) o).colour);
		}
	}

	public static List<Bag> read(Scanner sc) {
		List<Bag> bags = new ArrayList<>();

		while (sc.hasNextLine()) {
			Bag bag = new Bag(sc.next() + " " + sc.next());
			bags.add(bag);
			sc.next();
			sc.next();
			Arrays.stream(sc.nextLine().split(","))
					.map(b -> b.split(" "))
					.forEach(b -> {
						if (!b[1].equals("no"))
							bag.addInside(b[2] + " " + b[3], Integer.parseInt(b[1]));
					});
		}

		return bags;
	}

	public static int bagHolders(List<Bag> bags, String colour) {
		int size;
		Set<String> contain = new HashSet<>();
		contain.add(colour);
		do {
			size = contain.size();
			contain.addAll(contain.stream().flatMap(c -> bags.stream()
					.filter(b -> b.contains(c))
					.map(b -> b.colour)).collect(Collectors.toSet()));

		} while (contain.size() > size);

		return contain.size() - 1;
	}

	public static long bagContent(List<Bag> bags, String colour) {
		int index = bags.indexOf(new Bag(colour));
		if (index == -1)
			return 0L;

		Bag bag = bags.get(index);
		if (!bag.hasBags())
			return 0L;

		return bag.inside.keySet().stream()
				.mapToLong(k -> (bagContent(bags, k) + 1) * bag.inside.get(k))
				.sum();
	}

	public static void main(String[] args) {
		Scanner sc = Input.openFile(Year.AOC_2020, "day07.txt");

		List<Bag> bags = read(sc);

		System.out.println(bagHolders(bags, "shiny gold"));
		System.out.println(bagContent(bags, "shiny gold"));
	}

}
