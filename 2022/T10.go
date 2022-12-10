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
        ln := ""
        for _, v := range crt[40 * h : 40 * (h + 1)] {
            if v {
                ln = ln + "#"
            } else {
                ln = ln + "."
            }
        }
        fmt.Println(ln)
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

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    regX := 1
    prevX := 0
    cycles := 1
    crt := make([]bool, 40 * 6)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        stepcrt(crt, cycles, regX)
        prevX = regX
        switch (split[0]) {
            case "addx":
                stepcrt(crt, cycles + 1, regX)
                val, _ := strconv.Atoi(split[1])
                regX += val
                cycles += 2
            case "noop":
                cycles += 1
        }
        strength := regX * cycles
        if cycles % 40 == 20 || (split[0] == "addx" && cycles % 40 == 21)  {
            if cycles % 40 == 21 {
                strength = prevX * (cycles - 1)
            }
            pt1 += strength
        }
    }
    fmt.Println(pt1)
    crtdump(crt)
}

// vim: expandtab tw=99 colorcolumn=100
