package library

import java.io.BufferedReader
import java.io.File
import java.security.InvalidParameterException

private const val FILENAME = "day%02d/%s.txt"
private const val DEFAULT_FILE = "in"


fun getReader(year: Year, filename: String): BufferedReader =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()

fun getReader(year: Year, day: Int, trail: String = DEFAULT_FILE): BufferedReader {
    if (day < 1 || day > 25) {
        throw InvalidParameterException("Day must be between 1 and 25")
    }
    return getReader(year, String.format(FILENAME, day, trail))
}

fun readFirst(year: Year, filename: String): String =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()
        .use { it.readLine() }

fun readData(year: Year, filename: String): List<String> =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()
        .readLines()

fun readData(year: Year, day: Int, name: String = DEFAULT_FILE): List<String> {
    if (day < 1 || day > 25) {
        throw InvalidParameterException("Day must be between 1 and 25")
    }
    return readData(year, String.format(FILENAME, day, name))
}

fun readInts(year: Year, filename: String) : List<Int> =
    readData(year, filename).map { it.toInt() }

fun getFirstGroup(
    lines: List<String>,
    delimiter: String
): Pair<List<String>, List<String>> =
    Pair(
        lines.takeWhile { it != delimiter },
        lines.dropWhile { it != delimiter }.drop(1)
    )
