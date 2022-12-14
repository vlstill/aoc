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

type Point = utils.Point

func fill(map_ utils.Map[string], start utils.Point) (count int) {
    count = 0
    for {
        pos := start
        for {
            nxt := Point{pos.X, pos.Y + 1}
            if !map_.In(nxt) {
                return
            }
            if map_.At(nxt) == "o" || map_.At(nxt) == "#" {
                nxt = Point{pos.X - 1, pos.Y + 1}
            }
            if !map_.In(nxt) {
                return
            }
            if map_.At(nxt) == "o" || map_.At(nxt) == "#" {
                nxt = Point{pos.X + 1, pos.Y + 1}
            }
            if !map_.In(nxt) {
                return
            }
            if map_.At(nxt) == "o" || map_.At(nxt) == "#" {
                nxt = pos
                map_.Set(nxt, "o")
                count++
                if nxt.X == start.X && nxt.Y == start.Y {
                    return
                }
                break
            }
            pos = nxt
        }
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    map_ := utils.MakeMap[string](1000, 500, " ")
    start := utils.Point{500, 0}
    ymax := 0
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        from := utils.Point{}
        to := utils.Point{}
        first := true
        for i := 0; i < len(split); i += 2 {
            from = to
            xy := strings.Split(split[i], ",")
            x, _ := strconv.Atoi(xy[0])
            y, _ := strconv.Atoi(xy[1])
            ymax = utils.Max(ymax, y)
            to = utils.Point{x, y}
            if !first {
                map_.FillLine(from, to, "#")
            }
            first = false
        }
    }

    fmt.Println(fill(map_.Clone(), start))

    map_.FillLine(Point{0, ymax + 2}, Point{999, ymax + 2}, "#")

    fmt.Println(fill(map_, start))
    // map_.DumpSlice(Point{480, 0}, Point{520, 15})
}

// vim: expandtab tw=99 colorcolumn=100
