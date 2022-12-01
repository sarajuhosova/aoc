package library

import java.io.File

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
        lines.dropWhile { it != delimiter }.tail()
    )

fun <T> parseAsGrouped(
    data: List<String>,
    delimiter: String,
    mapper: (List<String>) -> T
): List<T> {
    return when (data.size) {
        0 -> emptyList()
        else -> {
            val (group, next) = getFirstGroup(data, delimiter)
            listOf(mapper(group)) + parseAsGrouped(next, delimiter, mapper)
        }
    }
}
