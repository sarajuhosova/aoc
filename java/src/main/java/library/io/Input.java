package library.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import library.DataType;
import library.Year;

public class Input {

	/**
	 * Creates a scanner for the input file of that day.
	 *
	 * @param  year     the year of the Advent of Code
	 * @param  fileName the name of the file
	 * @return          the scanner which will read the data
	 */
	public static Scanner openFile(Year year, String fileName) {
		try {
			return new Scanner(new File("src/main/resources/" + year.getDirectory() + "/" + fileName));
		} catch (FileNotFoundException e) {
			System.out.println("No such file found!");
			System.exit(1);
		}
		return null;
	}

	/**
	 * Reads the data from the relevant file to a stream of string.
	 *
	 * @param  year     the year of the Advent of Code
	 * @param  fileName the name of the file
	 * @return          the stream of strings containing the data
	 */
	public static Stream<String> readData(Year year, String fileName) {
		try {
			return Files.lines(Path.of("src/main/resources/" + year.getDirectory() + "/" + fileName));
		} catch (IOException e) {
			System.out.println("No such file found!");
			System.exit(1);
		}
		return null;
	}

	/**
	 * Reads the data from the relevant file to a stream of string.
	 *
	 * @param  year      the year of the Advent of Code
	 * @param  fileName  the name of the file
	 * @param  delimiter the delimiter on which to split the individual lines
	 * @return           the stream of strings containing the data
	 */
	public static Stream<String> readData(Year year, String fileName, String delimiter) {
		try {
			return Files.lines(Path.of("src/main/resources/" + year.getDirectory() + "/" + fileName))
					.map(l -> l.split(delimiter)).flatMap(Arrays::stream);
		} catch (IOException e) {
			System.out.println("No such file found!");
			System.exit(1);
		}
		return null;
	}

	/**
	 * Maps a stream of strings to a list of the desired data type.
	 *
	 * @param  data the stream containing the data
	 * @param  type the desired data type
	 * @param  <T>  the desired data type
	 * @return      a list containing the data in the desired data type
	 */
	@SuppressWarnings("unchecked")
	public static <T> List<T> collectDataToList(Stream<String> data, DataType type) {
		return (List<T>) data.map(type.getConverter()).collect(Collectors.toList());
	}

	/**
	 * Maps a stream of strings to a set of the desired data type.
	 *
	 * @param  data the stream containing the data
	 * @param  type the desired data type
	 * @param  <T>  the desired data type
	 * @return      a set containing the data in the desired data type
	 */
	@SuppressWarnings("unchecked")
	public static <T> Set<T> collectDataToSet(Stream<String> data, DataType type) {
		return (Set<T>) data.map(type.getConverter()).collect(Collectors.toSet());
	}

	@SuppressWarnings("unchecked")
	public static <T> List<T> collectDataToList(Scanner sc, DataType type, String delimiter) {
		sc.useDelimiter(delimiter);
		return collectDataToList(sc, type);
	}

	@SuppressWarnings("unchecked")
	public static <T> List<T> collectDataToList(Scanner sc, DataType type) {
		return type.getChecker().apply(sc) ? Stream.iterate(
				(T) type.getReader().apply(sc),
				s -> type.getChecker().apply(sc),
				s -> (T) type.getReader().apply(sc)).collect(Collectors.toList()) : new ArrayList<>();
	}

	@SuppressWarnings("unchecked")
	public static <T> Set<T> collectDataToSet(Scanner sc, DataType type, String delimiter) {
		sc.useDelimiter(delimiter);
		return collectDataToSet(sc, type);
	}

	@SuppressWarnings("unchecked")
	public static <T> Set<T> collectDataToSet(Scanner sc, DataType type) {
		return type.getChecker().apply(sc) ? Stream.iterate(
				(T) type.getReader().apply(sc),
				s -> type.getChecker().apply(sc),
				s -> (T) type.getReader().apply(sc)).collect(Collectors.toSet()) : new HashSet<>();
	}

}
