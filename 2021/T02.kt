fun get_final_position(input: List<Pair<String, Int>>, use_aim: Boolean): Pair<Int, Int> {
    var depth = 0
    var forward = 0
    var aim = 0
    for ((com, arg) in input) {
        when (com) {
            "forward" -> {
                forward += arg
                if (use_aim)
                    depth += aim * arg
            }
            "down" -> if (use_aim) aim += arg else depth += arg
            "up" ->   if (use_aim) aim -= arg else depth -= arg
        }
    }
    return Pair(forward, depth)
}

fun main() {
    var input = generateSequence(::readLine)
		     .map { x -> x.split(" ") }
		     .map { xs -> Pair(xs[0], xs[1].toInt()) }
		     .toList()
    val pos1 = get_final_position(input, false)
    System.err.println(pos1)
    println(pos1.first * pos1.second)

    val pos2 = get_final_position(input, true)
    System.err.println(pos2)
    println(pos2.first * pos2.second)
}
