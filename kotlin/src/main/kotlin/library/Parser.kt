package library

fun parseRegex(regex: String, line: String): List<String> =
    Regex(regex).find(line)!!.destructured.toList()

fun List<String>.parseRegex(regex: String): List<List<String>> =
    map { line -> parseRegex(regex, line) }

fun <T> List<String>.parseRegex(
    regex: String,
    formatter: (List<String>) -> T
): List<T> =
    parseRegex(regex).map(formatter)
