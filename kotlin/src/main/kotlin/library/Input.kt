package library

import java.io.File

fun readFirst(year: Year, filename: String): String =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()
        .use { it.readLine() }

fun readData(year: Year, filename: String): List<String> =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()
        .readLines()

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
