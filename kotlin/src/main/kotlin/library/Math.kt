package library

// https://www.baeldung.com/kotlin/lcm
fun lcm(a: Long, b: Long): Long {
    val larger = if (a > b) a else b
    val maxLcm = a * b
    var lcm = larger
    while (lcm <= maxLcm) {
        if (lcm % a == 0L && lcm % b == 0L) {
            return lcm
        }
        lcm += larger
    }
    return maxLcm
}


// https://www.baeldung.com/kotlin/lcm
fun List<Long>.lcm(): Long {
    var result = this[0]
    for (i in 1 until this.size) {
        result = lcm(result, this[i])
    }
    return result
}
