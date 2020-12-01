package library;

import java.util.Scanner;
import java.util.function.Function;

public enum DataType {
	INT(
		Scanner::hasNextInt, Scanner::nextInt, Integer::parseInt
	),
	LONG(
		Scanner::hasNextLong, Scanner::nextLong, Long::parseLong
	),
	WORD(
		Scanner::hasNext, (Scanner::next), w -> w
	),
	LINE(
		Scanner::hasNextLine, Scanner::nextLine, w -> w
	),
	FLOAT(
		Scanner::hasNextFloat, Scanner::nextFloat, Float::parseFloat
	),
	DOUBLE(
		Scanner::hasNextDouble, Scanner::nextDouble, Double::parseDouble
	);

	Function<Scanner, Boolean> checker;
	Function<Scanner, ?> reader;
	Function<String, ?> converter;

	DataType(Function<Scanner, Boolean> checker, Function<Scanner, ?> reader, Function<String, ?> converter) {
		this.checker = checker;
		this.reader = reader;
		this.converter = converter;
	}

	public Function<Scanner, Boolean> getChecker() {
		return checker;
	}

	public Function<Scanner, ?> getReader() {
		return reader;
	}

	public Function<String, ?> getConverter() {
		return converter;
	}
}
