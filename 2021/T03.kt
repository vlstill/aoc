fun Boolean.toInt() = if (this) 1 else 0

fun get_counts(data: List<List<Int>>): List<Int> {
    return data.reduce { a, b -> a.zip(b, { x, y -> x + y }) }
}

fun do_filtering(data: List<List<Int>>, look: Boolean): Int {
    var filtered = data;
    for (pos in 0..(data[0].size - 1) ) {
        val cnt1 = filtered.map { xs -> xs[pos] }.sum()
        val cnt0 = filtered.size - cnt1
        val value = ((cnt1 >= cnt0 && look) || (cnt1 < cnt0 && !look)).toInt()
        filtered = filtered.filter { xs -> xs[pos] == value }.toList()
        println(filtered.size)
        println(filtered)
        if (filtered.size == 1)
            return Integer.parseInt(filtered[0].map(Int::toString).joinToString(""), 2)
    }
    return 0
}

fun main() {
    val data = generateSequence(::readLine)
                  .map { x -> x.toList().map { c -> if (c == '1') 1 else 0 } }
                  .toList()
    val counts = get_counts(data)

    var gamma_s = ""
    var epsilon_s = ""
    for (c in counts) {
        if (c > data.size / 2) {
            gamma_s += "1"
            epsilon_s += "0"
        }
        else {
            gamma_s += "0"
            epsilon_s += "1"
        }
    }
    var gamma = Integer.parseInt(gamma_s, 2)
    var epsilon = Integer.parseInt(epsilon_s, 2)

    println(gamma * epsilon)
    println(do_filtering(data, true) * do_filtering(data, false))
}
