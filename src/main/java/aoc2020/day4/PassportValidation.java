package aoc2020.day4;

import java.util.*;
import java.util.stream.Collectors;

import library.Year;
import library.io.Input;
import library.enums.Attribute;

/**
 * You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your
 * passport. While these documents are extremely similar, North Pole Credentials aren't issued by a country
 * and therefore aren't actually valid documentation for travel in most of the world. It seems like you're not
 * the only one having problems, though; a very long line has formed for the automatic passport scanners, and
 * the delay could upset your travel itinerary. Due to some questionable network security, you realize you
 * might be able to solve both of these problems at the same time.
 */
public class PassportValidation {

	static Set<Attribute> attributes = Arrays.stream(Attribute.values()).collect(Collectors.toSet());
	static Map<String, Attribute> mapper = Arrays
			.stream(Attribute.values())
			.collect(Collectors.toMap(Attribute::getCode, a -> a));

	public static List<Map<Attribute, String>> read(Scanner sc) {
		return Arrays.stream(Input.readData(Year.AOC_2020, "day4.txt")
				.reduce("", (a, b) -> a += b + "\n")
				.split("\n\n")).map(
						p -> Arrays.stream(p.split(" |\n"))
								.map(a -> a.split(":"))
								.collect(Collectors.toMap(k -> mapper.get(k[0]), v -> v[1])))
				.collect(Collectors.toList());
	}

	/**
	 * The automatic passport scanners are slow because they're having trouble detecting which passports have
	 * all required fields. The expected fields are as follows:
	 *
	 * byr (Birth Year) iyr (Issue Year) eyr (Expiration Year) hgt (Height) hcl (Hair Color) ecl (Eye Color)
	 * pid (Passport ID) cid (Country ID)
	 *
	 * Passport data is validated in batch files (your puzzle input). Each passport is represented as a
	 * sequence of key:value pairs separated by spaces or newlines. Passports are separated by blank lines.
	 *
	 * @param  data the parse input data
	 * @return      Count the number of valid passports - those that have all required fields. Treat cid as
	 *              optional. In your batch file, how many passports are valid?
	 */
	public static long countAllAttributes(List<Map<Attribute, String>> data) {
		return data.stream().filter(s -> s.keySet().containsAll(attributes)).count();
	}

	/**
	 * The line is moving more quickly now, but you overhear airport security talking about how passports with
	 * invalid data are getting through. Better add some data validation, quick!
	 *
	 * You can continue to ignore the cid field, but each other field has strict rules about what values are
	 * valid for automatic validation:
	 *
	 * byr (Birth Year) - four digits; at least 1920 and at most 2002. iyr (Issue Year) - four digits; at
	 * least 2010 and at most 2020. eyr (Expiration Year) - four digits; at least 2020 and at most 2030. hgt
	 * (Height) - a number followed by either cm or in: If cm, the number must be at least 150 and at most
	 * 193. If in, the number must be at least 59 and at most 76. hcl (Hair Color) - a # followed by exactly
	 * six characters 0-9 or a-f. ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth. pid (Passport
	 * ID) - a nine-digit number, including leading zeroes. cid (Country ID) - ignored, missing or not.
	 *
	 * Your job is to count the passports where all required fields are both present and valid according to
	 * the above rules.
	 *
	 * @param  data the parse input data
	 * @return      Count the number of valid passports - those that have all required fields and valid
	 *              values. Continue to treat cid as optional. In your batch file, how many passports are
	 *              valid?
	 */
	public static long countValidAttributes(List<Map<Attribute, String>> data) {
		return data.stream()
				.filter(s -> s.keySet().containsAll(attributes))
				.filter(d -> d.keySet().stream()
						.allMatch(k -> k.getValidator().apply(d.get(k))))
				.count();
	}

	public static void main(String[] args) {
		Scanner sc = Input.openFile(Year.AOC_2020, "day4.txt");
		List<Map<Attribute, String>> data = read(sc);

		attributes.remove(Attribute.COUNTRY_ID);

		System.out.println(countAllAttributes(data));
		System.out.println(countValidAttributes(data));
	}

}
