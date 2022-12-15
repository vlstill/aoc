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

func pasgn(asgn string) int {
    splita := strings.Split(asgn, "=")
    val, _ := strconv.Atoi(splita[1])
    return val
}

func parse(str string) Point {
    splitc := strings.Split(str, ",")
    return Point{pasgn(splitc[0]), pasgn(splitc[1])}
}

func dist(a, b Point) int {
    return utils.Abs(a.X - b.X) + utils.Abs(a.Y - b.Y)
}

func filled(s2b map[Point]Point, line int) int {
    fill := make(utils.Set[int])
    for sen, bea := range s2b {
        d := dist(sen, bea)
        may := utils.Max(sen.Y, bea.Y) + d
        miy := utils.Min(sen.Y, bea.Y) - d
        if !(miy <= line && line <= may) {
            continue
        }
        for dx := -d; dx <= d; dx++ {
            dy := line - sen.Y
            if utils.Abs(dx) + utils.Abs(dy) > d {
                continue
            }
            y := sen.Y + dy
            x := sen.X + dx
            if y == line && (x != bea.X || y != bea.Y) {
                fill.Insert(x)
            }
        }
    }
    return len(fill)
}

func fill2D(s2b map[Point]Point, bound int) Point {
    fill := utils.MakeMap(bound, bound, false)
    for sen, bea := range s2b {
        d := dist(sen, bea)
        mix := utils.Min(sen.X, bea.X) - d
        miy := utils.Min(sen.Y, bea.Y) - d
        if miy > bound || mix > bound {
            continue
        }
        for dx := -d; dx <= d; dx++ {
            for dy := -d; dy <= d; dy++ {
                if utils.Abs(dx) + utils.Abs(dy) > d {
                    continue
                }
                pt := Point{sen.X + dx, sen.Y + dy}
                if fill.In(pt) {
                    fill.Set(pt, true)
                }
            }
        }
    }
    for y, line := range fill {
        for x, v := range line {
            if !v {
                return Point{x, y}
            }
        }
    }
    panic("not found")
}

func freq(p Point) int {
    return p.X * 4000000 + p.Y
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    map_ := make(map[Point]rune)
    s2b := make(map[Point]Point)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, ":")
        sensor := parse(split[0])
        beacon := parse(split[1])
        map_[sensor] = 'S'
        map_[beacon] = 'B'
        s2b[sensor] = beacon
        fmt.Println(sensor, beacon)
        use(split)
    }
    fmt.Println(s2b)

    fmt.Println(filled(s2b, 10))
    fmt.Println(filled(s2b, 2000000))
    fmt.Println(freq(fill2D(s2b, 4000000)))
}

// vim: expandtab tw=99 colorcolumn=100
