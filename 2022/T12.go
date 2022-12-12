package main

import (
    "bufio"
    "fmt"
    "os"
    "aoc/utils"
)

func steps(map_ utils.Map[int], from, to utils.Point) int {
    xd := map_.Xd()
    yd := map_.Yd()
    seen := make(map[utils.Point]int)
    seen[from] = 0
    queue := []utils.Point{from}

    mindist := xd * yd + 1

    for len(queue) > 0 {
        from := queue[0]
        distance := seen[from]
        if from == to {
            mindist = utils.Min(mindist, distance)
        }
        queue = queue[1:]
        h := map_.At(from)
        for _, dx := range []int{-1, 0, 1} {
            for _, dy := range []int{-1, 0, 1} {
                if utils.Abs(dx) + utils.Abs(dy) != 1 {
                    continue
                }
                nxt := utils.Point{from.X + dx, from.Y + dy}
                if !map_.In(nxt) {
                    continue
                }
                hnxt := map_.At(nxt)
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
    start := utils.Point{}
    end := utils.Point{}
    map_ := utils.Map[int]{}
    py := 0
    for scanner.Scan() {
        px := 0
        line := scanner.Text()
        out := []int{}
        for _, v := range line {
            if v == 'S' {
                out = append(out, 0)
                start = utils.Point{px, py}
            } else if v == 'E' {
                out = append(out, 25)
                end = utils.Point{px, py}
            } else {
                out = append(out, int(v) - int('a'))
            }
            px++
        }
        py++
        map_ = append(map_, out)
    }
    fmt.Println(steps(map_, start, end))

    xd := map_.Xd()
    yd := map_.Yd()
    pt2 := xd * yd + 1
    for x := 0; x < xd; x++ {
        for y := 0; y < yd; y++ {
            if map_[y][x] == 0 {
                pt2 = utils.Min(pt2, steps(map_, utils.Point{x, y}, end))
            }
        }
    }

    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
