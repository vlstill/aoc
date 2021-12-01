fun windowed_increases(data: List<Int>, window: Int): Int {
    var last: Int? = null
    var count = 0
    for (w in data.windowed(size = window).map { x -> x.sum() }) {
        if (last != null && last < w)
            ++count
        last = w
    }
    return count
}

fun main() {
    var radar = generateSequence(::readLine).map(String::toInt).toList()
    println(windowed_increases(radar, 1))
    println(windowed_increases(radar, 3))
}
