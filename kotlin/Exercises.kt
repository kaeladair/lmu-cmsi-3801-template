import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException

fun change(amount: Long): Map<Int, Long> {
    require(amount >= 0) { "Amount cannot be negative" }
    
    val counts = mutableMapOf<Int, Long>()
    var remaining = amount
    for (denomination in listOf(25, 10, 5, 1)) {
        counts[denomination] = remaining / denomination
        remaining %= denomination
    }
    return counts
}

// Write your first then lower case function here
fun <T> firstThenLowerCase(list: List<T>, predicate: (T) -> Boolean): String? {
    return list.firstOrNull(predicate)?.toString()?.lowercase()
}

// Write your say function here
class Say(private var _phrase: String = "") {
    fun and(word: String): Say {
        if (_phrase.isNotEmpty()) _phrase += " "
        _phrase += word
        return this
    }

    val phrase: String
        get() = _phrase
}

fun say(word: String = ""): Say = Say(word)

// Write your meaningfulLineCount function here
fun meaningfulLineCount(filename: String): Long {
    BufferedReader(FileReader(filename)).use { reader ->
        return reader.lines()
            .filter { it.trim().isNotEmpty() && !it.trim().startsWith("#") }
            .count()
    }
}

// Write your Quaternion data class here
data class Quaternion(val a: Double, val b: Double, val c: Double, val d: Double) {
    companion object {
        val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
        val I = Quaternion(0.0, 1.0, 0.0, 0.0)
        val J = Quaternion(0.0, 0.0, 1.0, 0.0)
        val K = Quaternion(0.0, 0.0, 0.0, 1.0)
    }

    init {
        require(!listOf(a, b, c, d).any { it.isNaN() }) { "Coefficients cannot be NaN" }
    }

    fun coefficients() = listOf(a, b, c, d)

    val conjugate: Quaternion
        get() = Quaternion(a, -b, -c, -d)

    operator fun plus(other: Quaternion) = Quaternion(a + other.a, b + other.b, c + other.c, d + other.d)

    operator fun times(other: Quaternion) = Quaternion(
        a * other.a - b * other.b - c * other.c - d * other.d,
        a * other.b + b * other.a + c * other.d - d * other.c,
        a * other.c - b * other.d + c * other.a + d * other.b,
        a * other.d + b * other.c - c * other.b + d * other.a
    )

    override fun toString(): String {
        val terms = listOf(a to "", b to "i", c to "j", d to "k")
            .filter { it.first != 0.0 }
            .mapIndexed { index, (coeff, unit) ->
                val sign = if (index == 0 || coeff < 0) "" else "+"
                "$sign$coeff$unit".replace(".0", "")
            }
        return terms.joinToString("").ifEmpty { "0" }
    }
}

// Write your Binary Search Tree interface and implementing classes here
sealed interface BinarySearchTree {
    val size: Int
    fun contains(value: String): Boolean
    fun insert(value: String): BinarySearchTree

    object Empty : BinarySearchTree {
        override val size = 0
        override fun contains(value: String) = false
        override fun insert(value: String) = Node(value, Empty, Empty)
        override fun toString() = "()"
    }

    data class Node(
        val value: String,
        val left: BinarySearchTree,
        val right: BinarySearchTree
    ) : BinarySearchTree {
        override val size = 1 + left.size + right.size
        override fun contains(value: String): Boolean = when {
            this.value == value -> true
            value < this.value -> left.contains(value)
            else -> right.contains(value)
        }
        override fun insert(value: String): BinarySearchTree = when {
            value < this.value -> copy(left = left.insert(value))
            value > this.value -> copy(right = right.insert(value))
            else -> this
        }
        override fun toString() = "($left$value$right)"
    }
}
