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

func move(rope []Point, dx, dy int) {
    rope[0].x += dx
    rope[0].y += dy
    for i := 1; i < len(rope); i++ {
        dx = utils.Signum(rope[i - 1].x - rope[i].x)
        dy = utils.Signum(rope[i - 1].y - rope[i].y)
        if utils.Abs(rope[i - 1].x - rope[i].x) > 1 {
            rope[i].x += dx
            if rope[i - 1].y != rope[i].y {
                if rope[i - 1].y > rope[i].y {
                    rope[i].y++
                } else {
                    rope[i].y--
                }
            }
        } else if utils.Abs(rope[i - 1].y - rope[i].y) > 1 {
            rope[i].y += dy
            if rope[i - 1].x != rope[i].x {
                if rope[i - 1].x > rope[i].x {
                    rope[i].x++
                } else {
                    rope[i].x--
                }
            }
        }
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    rope1 := []Point{Point{0, 0}, Point{0, 0}}
    rope2 := make([]Point, 10)
    seen := make(map[Point]int)
    seen2 := make(map[Point]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        use(split)
        cnt, _ := strconv.Atoi(split[1])
        fmt.Println(split[0], cnt)
        for i := 0; i < cnt; i++ {
            switch split[0] {
                case "R":
                    move(rope1, 1, 0)
                    move(rope2, 1, 0)
                case "L":
                    move(rope1, -1, 0)
                    move(rope2, -1, 0)

                case "U":
                    move(rope1, 0, 1)
                    move(rope2, 0, 1)
                case "D":
                    move(rope1, 0, -1)
                    move(rope2, 0, -1)
                default:
                    fmt.Println("invalid")
            }
            seen[rope1[1]] = 1
            seen2[rope2[9]] = 1
            fmt.Println(rope2)
        }
    }
    pt1 = len(seen)
    pt2 = len(seen2)
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
