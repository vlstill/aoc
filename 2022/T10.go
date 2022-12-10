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

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    regX := 1
    cycles := 1
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
        switch (split[0]) {
            case "addx":
                val, _ := strconv.Atoi(split[1])
                regX += val
                cycles += 2
            case "noop":
                cycles += 1
        }
    }
    fmt.Println("c", cycles)
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
