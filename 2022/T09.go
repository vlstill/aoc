package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
    "math/cmplx"
)

func move(rope []complex128, dir complex128) {
    rope[0] += dir
    for i := 1; i < len(rope); i++ {
        diff := rope[i - 1] - rope[i]
        if cmplx.Abs(diff) > 1.5 {
            rope[i] += complex(utils.Signum(real(diff)), utils.Signum(imag(diff)))
        }
    }
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    rope1 := make([]complex128, 2)
    rope2 := make([]complex128, 10)
    seen1 := make(map[complex128]int)
    seen2 := make(map[complex128]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        cnt, _ := strconv.Atoi(split[1])
        for i := 0; i < cnt; i++ {
            switch split[0] {
                case "R":
                    move(rope1, 1)
                    move(rope2, 1)
                case "L":
                    move(rope1, -1)
                    move(rope2, -1)
                case "U":
                    move(rope1, 1i)
                    move(rope2, 1i)
                case "D":
                    move(rope1, -1i)
                    move(rope2, -1i)
            }
            seen1[rope1[1]] = 1
            seen2[rope2[9]] = 1
        }
    }
    fmt.Println(len(seen1))
    fmt.Println(len(seen2))
}

// vim: expandtab tw=99 colorcolumn=100
