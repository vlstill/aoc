package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

func parse_range(in string) (int, int) {
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
        l0, h0 := parse_range(split[0])
        l1, h1 := parse_range(split[1])

        if ((l0 <= l1 && h0 >= h1) || (l1 <= l0 && h1 >= h0)) {
            pt1 += 1
            pt2 += 1
        } else if (l1 <= l0 && l0 <= h1) || (l1 <= h0 && h0 <= h1) ||
                  (l0 <= l1 && l1 <= h0) || (l0 <= h1 && h1 <= h0) {
            pt2 += 1
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
