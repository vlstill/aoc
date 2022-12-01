package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "aoc/utils"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    last := 0
    cals := make([]int, 0)
    for scanner.Scan() {
        line := scanner.Text()
        val, _ := strconv.Atoi(line)
        if line == "" {
            cals = append(cals, last)
            last = 0
        } else {
            last += val
        }
    }
    cals = append(cals, last)
    
    cals = utils.SortDesc(cals)
    fmt.Println(cals[0])
    fmt.Println(cals[0] + cals[1] + cals[2])
}

// vim: expandtab tw=99 colorcolumn=100
