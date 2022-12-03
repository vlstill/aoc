package main

import (
    "bufio"
    "fmt"
    "os"
    "aoc/utils"
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

func in_all(elves [][]int) int {
    var comm utils.Set[int] = nil
    for _, bag := range elves {
        if comm == nil {
            comm = utils.MkSet(bag)
        } else {
            comm = utils.Intersect(comm, utils.MkSet(bag))
        }
    }
    for k, _ := range comm {  // fuj
        return k
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

        pt1 += in_all([][]int{first, second})

        group = append(group, all)
        if len(group) == 3 {
            pt2 += in_all(group)
            group = make([][]int, 0, 3)
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
