package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
)

func use(_ interface{}) {
    if 4 == 2 {
        _ = utils.Min(4, 2)
        _ = strings.Split("", "")
        _, _ = strconv.Atoi("42")
    }
}

type Point struct {
    x int
    y int
}

func steps(mapa [][]int, from, to Point) int {
    xd := len(mapa[0])
    yd := len(mapa)
    seen := make(map[Point]int)
    seen[from] = 0
    queue := []Point{from}

    mindi := 10000000000

    for len(queue) > 0 {
        from := queue[0]
        distance := seen[from]
        // fmt.Println(from, distance, seen)
        if from == to {
            mindi = utils.Min(mindi, distance)
        }
        queue = queue[1:]
        h := mapa[from.y][from.x]
        for _, dx := range []int{-1, 0, 1} {
            for _, dy := range []int{-1, 0, 1} {
                if utils.Abs(dx) + utils.Abs(dy) != 1 {
                    continue
                }
                nxt := Point{from.x + dx, from.y + dy}
                if nxt.x < 0 || nxt.x >= xd || nxt.y < 0 || nxt.y >= yd {
                    // fmt.Println("skip", nxt, "out")
                    continue
                }
                hnxt := mapa[nxt.y][nxt.x]
                if hnxt - h > 1 {
                    // fmt.Println("skip", nxt, "h")
                    continue
                }
                if _, ok := seen[nxt]; ok {
                    // fmt.Println("skip", nxt, "seen")
                    continue
                }
                queue = append(queue, nxt)
                seen[nxt] = distance + 1
            }
        }
    }
    return mindi
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 10000000000
    start := Point{}
    end := Point{}
    mapa := [][]int{}
    py := 0
    for scanner.Scan() {
        px := 0
        line := scanner.Text()
        out := []int{}
        for _, v := range line {
            if v == 'S' {
                out = append(out, 0)
                start = Point{px, py}
            } else if v == 'E' {
                out = append(out, 25)
                end = Point{px, py}
            } else {
                out = append(out, int(v) - int('a'))
            }
            px++
        }
        py++
        mapa = append(mapa, out)
    }
    pt1 = steps(mapa, start, end)
    fmt.Println(mapa, start, end)
    fmt.Println(pt1)

    for x := 0; x < len(mapa[0]); x++ {
        for y := 0; y < len(mapa); y++ {
            if mapa[y][x] == 0 {
                pt2 = utils.Min(pt2, steps(mapa, Point{x, y}, end))
            }
        }
    }

    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
