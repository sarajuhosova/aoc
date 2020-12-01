package aoc2020.day1;

import java.util.*;

import library.DataType;
import library.Year;
import library.io.Input;

/**
 * After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a
 * tropical island. Surely, Christmas will go on without you. The tropical island has its own currency and is
 * entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call
 * them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find
 * fifty of these coins by the time you arrive so you can pay the deposit on your room. To save your vacation,
 * you need to get all fifty stars by December 25th. Collect stars by solving puzzles. Two puzzles will be
 * made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the
 * first. Each puzzle grants one star. Good luck!
 */
public class ExpenseReport {

	static Set<Integer> nums;

	/**
	 * The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they
	 * had left over from a past vacation. They offer you a second one if you can find three numbers in your
	 * expense report that meet the same criteria.
	 *
	 * @return In your expense report, what is the product of the three entries that sum to 2020?
	 */
	public static int getThreeMultipliers() {
		for (Integer i : nums) {
			for (Integer j : nums) {
				for (Integer k : nums) {
					if (i + j + k == 2020) {
						return i * j * k;
					}
				}
			}
		}
		return -1;
	}

	/**
	 * Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input);
	 * apparently, something isn't quite adding up. Specifically, they need you to find the two entries that
	 * sum to 2020 and then multiply those two numbers together.
	 *
	 * @return Find the two entries that sum to 2020; what do you get if you multiply them together?
	 */
	public static int getTwoMultipliers() {
		for (Integer i : nums) {
			for (Integer j : nums) {
				if (i + j == 2020) {
					return i * j;
				}
			}
		}
		return -1;
	}

	public static void main(String[] args) {
		System.out.println("Hello, Advent of Code 2020!");

		// load data
		nums = Input.collectDataToSet(Input.readData(Year.AOC_2020, "day1.txt"), DataType.INT);

		// Part 1
		System.out.println(getTwoMultipliers());

		// Part 2
		System.out.println(getThreeMultipliers());
	}

}
