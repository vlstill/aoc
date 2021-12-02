fun get_final_position(input: List<Pair<String, Int>>): Pair<Int, Int> {
    var depth = 0
    var forward = 0
    for ((com, arg) in input) {
	if (com == "forward")
	    forward += arg
	if (com == "down")
	    depth += arg
	if (com == "up")
	    depth -= arg
    }
    return Pair(forward, depth)
}

fun get_final_position_aim(input: List<Pair<String, Int>>): Pair<Int, Int> {
    var depth = 0
    var forward = 0
    var aim = 0
    for ((com, arg) in input) {
	if (com == "forward") {
	    forward += arg
	    depth += aim * arg
	}
	if (com == "down")
	    aim += arg
	if (com == "up")
	    aim -= arg
	// println("$depth $forward $aim")
    }
    return Pair(forward, depth)
}

fun main() {
    var input = generateSequence(::readLine)
		     .map { x -> x.split(" ") }
		     .map { xs -> Pair(xs[0], xs[1].toInt()) }
		     .toList()
    val pos1 = get_final_position(input)
    println(pos1)
    println(pos1.first * pos1.second)
    val pos2 = get_final_position_aim(input)
    println(pos2)
    println(pos2.first * pos2.second)
}
