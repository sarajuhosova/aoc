package library

import java.util.*

enum class Year {
    _2016,
    _2018,
    _2019,
    _2020,
    _2021,
    _2022,
    _2023,
    _2024;

    val directory: String
        get() = name
            .lowercase(Locale.getDefault())
            .replace("_", "aoc")
}
