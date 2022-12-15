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

type Interval struct {
    From int
    To int
}

type IntervalSet []Interval

func MakeISet(from, to int) IntervalSet {
    return IntervalSet([]Interval{Interval{from, to}})
}

func (self *IntervalSet) Remove(from, to int) {
//    fmt.Println("remove", from, to)
    out := IntervalSet([]Interval{})
    for _, v := range *self {
        if v.To < from {
            out = append(out, v)
//            fmt.Println("a")
        } else if v.From > to {
//            fmt.Println("b")
            out = append(out, v)
        } else if v.From >= from && v.To <= to {
//            fmt.Println("c")
            continue // skip
        } else {
            if from <= v.To && v.From <= from - 1 {
                out = append(out, Interval{v.From, from - 1})
//                fmt.Println("d", out)
            }
            if to < v.To {
                out = append(out, Interval{to + 1, v.To})
//                fmt.Println("e", out)
            }
        }
    }
    *self = out
}

func (self IntervalSet) Size() int {
    out := 0
    for _, v := range self {
        out += v.To - v.From + 1
    }
    return out
}

func filled(s2b map[Point]Point, line int, xbound int) IntervalSet {
    fill := MakeISet(0, xbound)
    for sen, bea := range s2b {
        d := dist(sen, bea)
        disy := utils.Abs(line - sen.Y)
        if disy > d {
            continue
        }
        disx := d - disy
        fill.Remove(sen.X - disx, sen.X + disx)
//        fmt.Println(fill)
    }
    return fill
}

func fill2D(s2b map[Point]Point, bound int) Point {
    for y := 0; y <= bound; y++ {
        line := filled(s2b, y, bound)
        if len(line) == 1 {
            return Point{line[0].From, y}
        }
        fmt.Print(".")
    }
    panic("ei")
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
//        fmt.Println(sensor, beacon)
        use(split)
    }
//    fmt.Println(s2b)

//    fmt.Println(1000000001 - filled(s2b, 10, 1000000000).Size())
//    fmt.Println(filled(s2b, 2000000))
    fmt.Println(freq(fill2D(s2b, 4000000)))
}

// vim: expandtab tw=99 colorcolumn=100
