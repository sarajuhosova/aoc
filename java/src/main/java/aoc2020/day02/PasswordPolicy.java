package aoc2020.day02;

import java.util.*;

import library.Year;
import library.io.Input;

/**
 * Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is
 * via toboggan. The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong
 * with our computers; we can't log in!" You ask if you can take a look. Their password database seems to be a
 * little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate
 * Policy that was in effect when they were chosen. To try to debug the problem, they have created a list
 * (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that
 * password was set.
 */
public class PasswordPolicy {

	static class Policy {
		int a;
		int b;
		char c;
		String password;

		public Policy(int a, int b, char c, String password) {
			this.a = a;
			this.b = b;
			this.c = c;
			this.password = password;
		}
	}

	private static List<Policy> read(Scanner sc) {
		List<Policy> policies = new ArrayList<>();
		while (sc.hasNextLine()) {
			String[] parsed = sc.nextLine().split(" ");
			String[] num = parsed[0].split("-");

			policies.add(new Policy(
					Integer.parseInt(num[0]),
					Integer.parseInt(num[1]),
					parsed[1].charAt(0),
					parsed[2]));
		}
		return policies;
	}

	/**
	 * Each line gives the password policy and then the password. The password policy indicates the lowest and
	 * highest number of times a given letter must appear for the password to be valid. For example, 1-3 a
	 * means that the password must contain a at least 1 time and at most 3 times.
	 *
	 * @param  policies the parsed puzzle input
	 * @return          How many passwords are valid according to their policies?
	 */
	public static long amountPolicy(List<Policy> policies) {
		return policies.stream()
				.filter(p -> {
					long amount = p.password.chars()
							.mapToObj(ch -> (char) ch)
							.filter(ch -> ch.equals(p.c))
							.count();
					return amount >= p.a && amount <= p.b;
				}).count();
	}

	/**
	 * While it appears you validated the passwords correctly, they don't seem to be what the Official
	 * Toboggan Corporate Authentication System is expecting. The shopkeeper suddenly realizes that he just
	 * accidentally explained the password policy rules from his old job at the sled rental place down the
	 * street! The Official Toboggan Corporate Policy actually works a little differently. Each policy
	 * actually describes two positions in the password, where 1 means the first character, 2 means the second
	 * character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
	 * Exactly one of these positions must contain the given letter. Other occurrences of the letter are
	 * irrelevant for the purposes of policy enforcement.
	 *
	 * @param  policies the parsed puzzle input
	 * @return          How many passwords are valid according to the new interpretation of the policies?
	 */
	public static long positionPolicy(List<Policy> policies) {
		return policies.stream()
				.filter(p -> (p.password.charAt(p.a - 1) == p.c) ^ (p.password.charAt(p.b - 1) == p.c))
				.count();
	}

	public static void main(String[] args) {
		// read and parse data
		List<Policy> policies = read(Input.openFile(Year._2020, "day02.txt").useDelimiter(" "));

		// Part 1
		System.out.println(amountPolicy(policies));

		// Part 2
		System.out.println(positionPolicy(policies));
	}

}
