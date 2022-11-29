package library

import java.io.File

fun readData(year: Year, filename: String): List<String> =
    File("src/main/resources/" + year.directory + "/" + filename)
        .bufferedReader()
        .readLines()

fun readInts(year: Year, filename: String) : List<Int> =
    readData(year, filename).map { it.toInt() }
