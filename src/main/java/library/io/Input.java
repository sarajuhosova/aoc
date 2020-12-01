package library.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import library.Year;

public class Input {

	public static Scanner readData(Year year, String fileName) {
		try {
			return new Scanner(new File("src/main/resources/" + year.getDirectory() + "/" + fileName));
		} catch (FileNotFoundException e) {
			System.out.println("No such file found!");
			System.exit(1);
		}
		return null;
	}

}
