package aoc2016.day02

import java.lang.Integer.max
import java.lang.Integer.min

enum class Move(val move: (Key) -> Key) {
    U( { Key(max(0, it.row - 1), it.col) } ),
    D( { Key(min(2, it.row + 1), it.col) } ),
    L( { Key(it.row, max(0, it.col - 1)) } ),
    R( { Key(it.row, min(2, it.col + 1)) } );
}

data class Key(
    val row: Int = 1,
    val col: Int = 1
) {
    val value = (row * 3) + col + 1
}
