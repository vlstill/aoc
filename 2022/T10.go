package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
)

func crtdump(crt []bool) {
    for h := 0; h < 6; h++ {
        for _, v := range crt[40 * h : 40 * (h + 1)] {
            if v {
                fmt.Print("#")
            } else {
                fmt.Print(" ")
            }
        }
        fmt.Print("\n")
    }
}

func stepcrt(crt []bool, cycle, regX int) {
    cycle--  // AOC indexes are 1-based
    posX := cycle % 40
    pos := cycle % (6 * 40)
    if utils.Abs(posX - regX) <= 1 {
        crt[pos] = true
    }
}

func strength(cycle, regX int) int {
    if cycle % 40 != 20 {
        return 0
    }
    return regX * cycle
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    regX := 1
    cycle := 1
    crt := make([]bool, 40 * 6)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        stepcrt(crt, cycle, regX)
        switch split[0] {
            case "addx":
                stepcrt(crt, cycle + 1, regX)
                pt1 += strength(cycle + 1, regX)
                val, _ := strconv.Atoi(split[1])
                regX += val
                cycle += 2
            case "noop":
                cycle += 1
        }
        pt1 += strength(cycle, regX)
    }
    fmt.Println(pt1)
    crtdump(crt)
}

// vim: expandtab tw=99 colorcolumn=100
