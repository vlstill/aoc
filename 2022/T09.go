package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
)

type Point struct {
    x int
    y int
}

func move(rope []Point, dx, dy int) {
    rope[0].x += dx
    rope[0].y += dy
    for i := 1; i < len(rope); i++ {
        if utils.Abs(rope[i - 1].x - rope[i].x) > 1 ||
           utils.Abs(rope[i - 1].y - rope[i].y) > 1 {
            rope[i].x += utils.Signum(rope[i - 1].x - rope[i].x)
            rope[i].y += utils.Signum(rope[i - 1].y - rope[i].y)
        }
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    rope1 := []Point{Point{0, 0}, Point{0, 0}}
    rope2 := make([]Point, 10)
    seen1 := make(map[Point]int)
    seen2 := make(map[Point]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        cnt, _ := strconv.Atoi(split[1])
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
            }
            seen1[rope1[1]] = 1
            seen2[rope2[9]] = 1
        }
    }
    fmt.Println(len(seen1))
    fmt.Println(len(seen2))
}

// vim: expandtab tw=99 colorcolumn=100
