package main

import (
    "bufio"
    "fmt"
    "os"
    "aoc/utils"
)

func parse(line string) (out []int) {
    out = make([]int, len(line))
    for i, v := range line {
        out[i] = int(v) - int('0')
    }
    return
}

func vis(matrix [][]int, x, y, dx, dy int) bool {
    ref := matrix[x][y]

    x += dx
    y += dy
    for x >= 0 && y >= 0 && x < len(matrix) && y < len(matrix[0]) {
        if matrix[x][y] >= ref {
            return false
        }
        x += dx
        y += dy
    }
    return true
}

func visible(matrix [][]int, i, j int) bool {
    return vis(matrix, i, j, -1, 0) || vis(matrix, i, j, 1, 0) || vis(matrix, i, j, 0, -1) || vis(matrix, i, j, 0, 1)
}

func dis(matrix [][]int, x, y, dx, dy int) int {
    ref := matrix[x][y]

    x += dx
    y += dy
    cnt := 0
    for ;x >= 0 && y >= 0 && x < len(matrix) && y < len(matrix[0]); cnt ++ {
        if matrix[x][y] >= ref {
            return cnt + 1
        }
        x += dx
        y += dy
    }
    return cnt
}

func vidis(matrix [][]int, i, j int) int {
    return dis(matrix, i, j, -1, 0) * dis(matrix, i, j, 1, 0) * dis(matrix, i, j, 0, -1) * dis(matrix, i, j, 0, 1)
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    matrix := make([][]int, 0)
    for scanner.Scan() {
        line := scanner.Text()
        matrix = append(matrix, parse(line))
    }

    for i := 0; i < len(matrix); i++ {
        for j := 0; j < len(matrix[0]); j++ {
            if visible(matrix, i, j) {
                pt1++
            }
            pt2 = utils.Max(pt2, vidis(matrix, i, j))
        }
    }


    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
