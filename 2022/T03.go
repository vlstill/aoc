package main

import (
    "bufio"
    "fmt"
    "os"
)

func decode(str string) (out []int) {
    out = make([]int, len(str))
    for i, v := range str {
        if ('a' <= v && v <= 'z') {
            out[i] = int(v) - 'a' + 1
        } else {
            out[i] = int(v) - 'A' + 27
        }
    }
    return
}

func misplaced(first, second []int) int {
    for _, a := range first {
        for _, b := range second {
            if a == b {
                return a
            }
        }
    }
    return 0
}

func find_badge(elves [][]int) int {
    for _, a := range elves[0] {
        for _, b := range elves[1] {
            for _, c := range elves[2] {
                if a == b && b == c {
                    return a
                }
            }
        }
    }
    return 0
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    group := make([][]int, 0, 3)
    for scanner.Scan() {
        line := scanner.Text()
        all := decode(line)
        first := decode(line[0:len(line)/2])
        second := decode(line[len(line)/2:])

        pt1 += misplaced(first, second)

        group = append(group, all)
        if len(group) == 3 {
            pt2 += find_badge(group)
            group = make([][]int, 0, 3)
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
