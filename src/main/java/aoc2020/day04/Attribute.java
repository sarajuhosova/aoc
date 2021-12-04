package aoc2020.day04;

import java.util.Set;
import java.util.function.Function;

public enum Attribute {
	BIRTH_YEAR(
		"byr", s -> s.matches("\\d{4}") && Integer.parseInt(s) >= 1920 && Integer.parseInt(s) <= 2002
	),
	ISSUE_YEAR(
		"iyr", s -> s.matches("\\d{4}") && Integer.parseInt(s) >= 2010 && Integer.parseInt(s) <= 2020
	),
	EXPIRATION_YEAR(
		"eyr", s -> s.matches("\\d{4}") && Integer.parseInt(s) >= 2020 && Integer.parseInt(s) <= 2030
	),
	HEIGHT(
		"hgt", s -> {
			String value = s.substring(0, s.length() - 2);
			int val;
			switch (s.substring(s.length() - 2)) {
				case "cm":
					val = Integer.parseInt(value);
					return val >= 150 && val <= 193;
				case "in":
					val = Integer.parseInt(value);
					return val >= 59 && val <= 76;
				default:
					return false;
			}
		}
	),
	HAIR_COLOUR(
		"hcl", s -> s.matches("#[0-9a-f]{6}")
	),
	EYE_COLOUR(
		"ecl", s -> Set.of("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s)
	),
	PASSPORT_ID(
		"pid", s -> s.matches("\\d{9}")
	),
	COUNTRY_ID(
		"cid", s -> true
	);

	String code;
	Function<String, Boolean> validator;

	Attribute(String code, Function<String, Boolean> validator) {
		this.code = code;
		this.validator = validator;
	}

	public String getCode() {
		return code;
	}

	public Function<String, Boolean> getValidator() {
		return validator;
	}
}
