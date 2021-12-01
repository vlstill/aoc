fun windowed_increases(data: List<Int>, window: Int): Int {
    var last: Int? = null
    var count = 0
    for (w in data.windowed(size = window)) {
        if (last != null && last!! < w.sum())
            ++count
        last = w.sum()
    }
    return count
}

fun main() {
    var radar = ArrayList<Int>();
    var sweep: Int? = readLine()?.toInt()
    while (sweep != null) {
        radar.add(sweep);
        sweep = readLine()?.toInt()
    }
    println(windowed_increases(radar, 1))
    println(windowed_increases(radar, 3))
}
