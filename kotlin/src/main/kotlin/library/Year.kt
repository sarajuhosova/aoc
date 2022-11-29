package library

import java.util.*

enum class Year {
    _2021,
    _2022;

    val directory: String
        get() = name
            .lowercase(Locale.getDefault())
            .replace("_", "aoc")
}