fun Boolean.toInt() = if (this) 1 else 0
fun String.toInt(base: Int) = Integer.parseInt(this, base)

fun do_filtering(data: List<List<Int>>, look: Boolean): Int {
    var filtered = data;
    for (pos in 0..(data[0].size - 1) ) {
        val cnt1 = filtered.map { xs -> xs[pos] }.sum()
        val cnt0 = filtered.size - cnt1
        val value = ((cnt1 >= cnt0 && look) || (cnt1 < cnt0 && !look)).toInt()
        filtered = filtered.filter { xs -> xs[pos] == value }.toList()
        if (filtered.size == 1)
            return Integer.parseInt(filtered[0].map(Int::toString).joinToString(""), 2)
    }
    return 0
}

fun main() {
    val data = generateSequence(::readLine)
                  .map { x -> x.toList().map { c -> if (c == '1') 1 else 0 } }
                  .toList()
    val counts = data.reduce { a, b -> a.zip(b, { x, y -> x + y }) }

    val larger = counts.map { c -> c > data.size / 2 }.toList()
    val gamma = larger.map { x -> if (x) "1" else "0" }.joinToString("").toInt(2)
    val epsilon = larger.map { x -> if (x) "0" else "1" }.joinToString("").toInt(2)

    println(gamma * epsilon)
    println(do_filtering(data, true) * do_filtering(data, false))
}
