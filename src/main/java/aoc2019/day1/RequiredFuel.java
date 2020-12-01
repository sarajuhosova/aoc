package aoc2019.day1;

import java.util.stream.LongStream;

import library.Year;
import library.io.Input;

public class RequiredFuel {

	public static long getOneLineRequiredFuelFuel(long mass) {
		return LongStream.iterate(mass, m -> m > 0, RequiredFuel::getRequiredFuel).sum() - mass;
	}

	public static long getRequiredFuelFuel(long mass) {
		if (mass <= 0)
			return 0;
		long fuel = getRequiredFuel(mass);
		return fuel + getRequiredFuelFuel(fuel);
	}

	public static long getRequiredFuel(long mass) {
		return (mass / 3) - 2;
	}

	public static void main(String[] args) {
		long mass = Input.readData(Year.AOC_2019, "day1.txt").nextInt();

		// Part 1
		System.out.println(getRequiredFuel(mass));

		// Part 2
		System.out.println(getRequiredFuelFuel(mass));

		// Part 2 One-Line
		System.out.println(getOneLineRequiredFuelFuel(mass));
	}

}
