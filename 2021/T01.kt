fun main() {
    var last: Int? = readLine()?.toInt()
    var increases = 0
    do {
        val now: Int? = readLine()?.toInt()
        if (now == null)
            break;
        if (now > last!!)
            increases++
        last = now
    } while (true);
    println(increases)
}
