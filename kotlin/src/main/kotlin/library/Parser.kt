package library

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

fun parseRegex(regex: String, line: String): List<String> =
    Regex(regex).find(line)!!.destructured.toList()

fun List<String>.parseRegex(regex: String): List<List<String>> =
    map { line -> parseRegex(regex, line) }

fun <T> List<String>.parseRegex(
    regex: String,
    formatter: (List<String>) -> T
): List<T> =
    parseRegex(regex).map(formatter)
