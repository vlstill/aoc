fun Boolean.toInt() = if (this) 1 else 0

typealias Board = MutableList<MutableList<Int?>>

fun getBoard(): Board? {
    var is_fist = true
    var board: Board = ArrayList<MutableList<Int?>>()
    while (true) {
        val line = readLine()
        if (line == null && is_fist)
            return null
        if (line == null || line == "")
            return board
        val row: MutableList<Int?> = line.split(" ").filter { x -> x.length > 0 }
                                         .map(String::toInt).toMutableList()
        board.add(row)
        is_fist = false
    }
}

fun Board.has_empty_row() = this.any { r -> r.filter { x -> x != null }.toList().size == 0 }
fun Board.transpose() = (0..this.size - 1).map { j -> (0..this.size - 1).map{ i -> this[i][j] }.toMutableList() }.toMutableList()

fun place(board: Board, number: Int): Boolean {
    for (i in 0..board.size - 1) {
        for (j in 0..board.size - 1) {
            if (board[i][j] == number)
                board[i][j] = null
        }
    }

    return (board.has_empty_row() || board.transpose().has_empty_row())
}

fun score(n: Int, board: Board): Int {
    return n * board.map { r -> r.filter { x -> x != null }.map { x -> x!! }.sum() }.sum()
}

fun play(boards_: List<Board>, numbers: List<Int>, to_loose: Boolean): Int? {
    var boards: MutableList<Board?> = boards_.toMutableList()
    var won = 0
    for (n in numbers) {
        for (i in 0..boards.size - 1) {
            val board = boards[i]
            if (board != null && place(board, n)) {
                if (!to_loose)
                    return score(n, board)
                else {
                    won++
                    if (won == boards.size) {
                        return score(n, board)
                    }
                }
                boards[i] = null
            }
        }
    }
    return null
}

fun main() {
    val numbers = readLine()!!.split(",").map(String::toInt).toList()
    readLine()

    val boards = generateSequence(::getBoard).toList()

    println(play(boards, numbers, false)!!)
    println(play(boards, numbers, true)!!)
}
