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
    fmt.Println("")
}

func stepcrt(crt []bool, cycle, regX int) {
    posX := cycle % 40
    pos := cycle % (6 * 40)
    if utils.Abs(posX - regX) <= 1 {
        crt[pos] = true
    } /*else {
        crt[pos] = false
    }*/
    fmt.Println("cycle", cycle)
    crtdump(crt)
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    regX := 1
    cycles := 0
    crt := make([]bool, 40 * 6)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        use(split)
        strength := regX * cycles
        if cycles % 40 == 20 || (split[0] == "addx" && cycles % 40 == 19)  {
            if cycles % 40 == 19 {
                strength += regX
            }
            fmt.Println(cycles, regX, strength)
            pt1 += strength
        }
        stepcrt(crt, cycles, regX)
        switch (split[0]) {
            case "addx":
                stepcrt(crt, cycles + 1, regX)
                val, _ := strconv.Atoi(split[1])
                regX += val
                cycles += 2
            case "noop":
                cycles += 1
        }
    }
    fmt.Println("c", cycles)
    fmt.Println(pt1)
    crtdump(crt)
}

// vim: expandtab tw=99 colorcolumn=100
