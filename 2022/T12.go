package main

import (
    "bufio"
    "fmt"
    "os"
    "aoc/utils"
)

type Point struct {
    x int
    y int
}

type Map [][]int

func (self Map) xd() int {
    return len(self[0])
}

func (self Map) yd() int {
    return len(self)
}

func (self Map) at(p Point) int {
    return self[p.y][p.x]
}

func steps(map_ Map, from, to Point) int {
    xd := map_.xd()
    yd := map_.yd()
    seen := make(map[Point]int)
    seen[from] = 0
    queue := []Point{from}

    mindist := xd * yd + 1

    for len(queue) > 0 {
        from := queue[0]
        distance := seen[from]
        if from == to {
            mindist = utils.Min(mindist, distance)
        }
        queue = queue[1:]
        h := map_.at(from)
        for _, dx := range []int{-1, 0, 1} {
            for _, dy := range []int{-1, 0, 1} {
                if utils.Abs(dx) + utils.Abs(dy) != 1 {
                    continue
                }
                nxt := Point{from.x + dx, from.y + dy}
                if nxt.x < 0 || nxt.x >= xd || nxt.y < 0 || nxt.y >= yd {
                    continue
                }
                hnxt := map_.at(nxt)
                if hnxt - h > 1 {
                    continue
                }
                if _, ok := seen[nxt]; ok {
                    continue
                }
                queue = append(queue, nxt)
                seen[nxt] = distance + 1
            }
        }
    }
    return mindist
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    start := Point{}
    end := Point{}
    map_ := Map{}
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
        map_ = append(map_, out)
    }
    fmt.Println(steps(map_, start, end))

    xd := map_.xd()
    yd := map_.yd()
    pt2 := xd * yd + 1
    for x := 0; x < xd; x++ {
        for y := 0; y < yd; y++ {
            if map_[y][x] == 0 {
                pt2 = utils.Min(pt2, steps(map_, Point{x, y}, end))
            }
        }
    }

    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
