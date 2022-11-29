package library;

public enum Year {
    _2020,
    _2019,
    _2021,
    _2022;

	public String getDirectory() {
		return this.name()
                .toLowerCase()
                .replace("_", "aoc");
	}
}
