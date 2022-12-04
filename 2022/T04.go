package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

func rng(in string) (int, int) {
    split := strings.Split(in, "-")
    a, _ := strconv.Atoi(split[0])
    b, _ := strconv.Atoi(split[1])
    return a, b
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, ",")
        a0, b0 := rng(split[0])
        a1, b1 := rng(split[1])

        if ((a0 <= a1 && b0 >= b1) || (a1 <= a0 && b1 >= b0)) {
            pt1 += 1
            pt2 += 1
        } else if (a1 <= a0 && a0 <= b1) || (a1 <= b0 && b0 <= b1) || (a0 <= a1 && a1 <= b0) || (a0 <= b1 && b1 <= b0) {
            pt2 += 1
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
