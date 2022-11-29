package aoc2020.day05;

import java.util.Set;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;

/**
 * You board your plane only to discover a new problem: you dropped your boarding pass! You aren't sure which
 * seat is yours, and all of the flight attendants are busy with the flood of people that suddenly made it
 * through passport control.
 *
 * You write a quick program to use your phone's camera to scan all of the nearby boarding passes (your puzzle
 * input); perhaps you can find your seat through process of elimination.
 *
 * Instead of zones or groups, this airline uses binary space partitioning to seat people. A seat might be
 * specified like FBFBBFFRLR, where F means "front", B means "back", L means "left", and R means "right".
 *
 * The first 7 characters will either be F or B; these specify exactly one of the 128 rows on the plane
 * (numbered 0 through 127). Each letter tells you which half of a region the given seat is in. Start with the
 * whole list of rows; the first letter indicates whether the seat is in the front (0 through 63) or the back
 * (64 through 127). The next letter indicates which half of that region the seat is in, and so on until
 * you're left with exactly one row.
 *
 * The last three characters will be either L or R; these specify exactly one of the 8 columns of seats on the
 * plane (numbered 0 through 7). The same process as above proceeds again, this time with only three steps. L
 * means to keep the lower half, while R means to keep the upper half.
 *
 * Every seat also has a unique seat ID: multiply the row by 8, then add the column.
 */
public class SeatFinder {

	public static String parseBack(int i) {
		return Integer.toBinaryString(i).replace('1', 'B')
				.replace('0', 'F')
				.replace('1', 'R')
				.replace('0', 'L');
	}

	/**
	 * Find the highest boarding pass.
	 *
	 * @return As a sanity check, look through your list of boarding passes. What is the highest seat ID on a
	 *         boarding pass?
	 */
	public static int highestSeat() {
		return Input.readData(Year._2020, "day5.txt")
				.map(s -> s.replace('B', '1')
						.replace('F', '0')
						.replace('R', '1')
						.replace('L', '0'))
				.map(s -> Integer.parseInt(s, 2))
				.max(Integer::compareTo).get();
	}

	/**
	 * It's a completely full flight, so your seat should be the only missing boarding pass in your list.
	 * However, there's a catch: some of the seats at the very front and back of the plane don't exist on this
	 * aircraft, so they'll be missing from your list as well.
	 *
	 * Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in
	 * your list.
	 *
	 * @return What is the ID of your seat?
	 */
	public static int leftoverSeat() {
		Set<Integer> data = Input.readData(Year._2020, "day05.txt")
				.map(s -> s.replace('B', '1')
						.replace('F', '0')
						.replace('R', '1')
						.replace('L', '0'))
				.map(s -> Integer.parseInt(s, 2))
				.collect(Collectors.toSet());

		for (int seat = 1; seat < Integer.parseInt("1111111111", 2); seat++) {
			if (!data.contains(seat)) {
				if (data.contains(seat - 1) && data.contains(seat + 1)) {
					return seat;
				}
			}
		}

		return -1;
	}

	public static void main(String[] args) {
		System.out.println(highestSeat());
		System.out.println(leftoverSeat());
	}

}
