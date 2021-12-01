fun Boolean.toInt() = if (this) 1 else 0

fun windowed_increases(data: List<Int>, window: Int): Int {
    return data.windowed(size = window)
               .map { x -> x.sum() }
               .zipWithNext { a, b -> (a < b).toInt() }
               .sum()
}

fun main() {
    var radar = generateSequence(::readLine).map(String::toInt).toList()
    println(windowed_increases(radar, 1))
    println(windowed_increases(radar, 3))
}
