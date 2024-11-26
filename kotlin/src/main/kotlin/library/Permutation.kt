package library

fun <T> permutations(list: List<T>): Set<List<T>> {
    if (list.isEmpty()) return setOf(emptyList())

    val result: MutableSet<List<T>> = mutableSetOf()
    for (i in list.indices) {
        permutations(list - list[i]).forEach{
                item -> result.add(item + list[i])
        }
    }
    return result
}
