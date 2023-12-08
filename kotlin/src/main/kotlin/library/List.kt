package library

class EmptyListException: RuntimeException()

fun <T> List<T>.tail() = if (isEmpty()) throw EmptyListException() else drop(1)
fun <T> List<T>.head() = first()

fun <T> List<T>.removeFirst(value: T): List<T> =
    when (this.size) {
        0 -> emptyList()
        else -> {
            val head = head()
            if (head == value) tail() else listOf(head) + tail().removeFirst(value)
        }
    }

fun <T> List<T>.rotated(i: Long): List<T> {
    val index = (i % this.size).toInt()
    return this.subList(index, this.size) + this.subList(0, index)
}

fun List<Long>.sum() = fold(0L) { acc, l -> l + acc }

fun List<Int>.product(): Int = fold(1) { acc, l -> l * acc }
fun List<Int>.longProduct(): Long = fold(1L) { acc, l -> l * acc }
fun List<Long>.product() = fold(1L) { acc, l -> l * acc }
