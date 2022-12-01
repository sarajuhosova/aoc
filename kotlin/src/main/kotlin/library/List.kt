package library

fun <T> List<T>.tail() = drop(1)
fun <T> List<T>.head() = first()

fun <T> List<T>.removeFirst(value: T): List<T> =
    when (this.size) {
        0 -> emptyList()
        else -> {
            val head = head()
            if (head == value) tail() else listOf(head) + tail().removeFirst(value)
        }
    }

fun List<Long>.sumOf() = fold(0L) { acc: Long, l: Long -> l + acc }
