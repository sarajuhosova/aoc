package library;

public enum Year {
	AOC_2019(
		"2019"
	),
	AOC_2020(
		"2020"
	);

	String yearString;

	Year(String string) {
		this.yearString = string;
	}

	public String getDirectory() {
		return "aoc" + yearString;
	}
}
