package library

fun <T> List<T>.tail() = drop(1)
fun <T> List<T>.head() = first()