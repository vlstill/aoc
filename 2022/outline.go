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
    for scanner.Scan() {
        line := scanner.Text()
        val, _ := strconv.Atoi(line)


    }
    fmt.Println("a1")
    fmt.Println("a2")
}

// vim: expandtab tw=99 colorcolumn=100
