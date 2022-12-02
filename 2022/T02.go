package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
//    "strconv"
//    "aoc/utils"
)

func eval(choice, other int) int {
    if choice == other {
        return choice + 3
    } else if (choice - other == 1 || other - choice == 2) {
        return choice + 6
    }
    return choice
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    score := 0
    score2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        // val, _ := strconv.Atoi(line)

        split := strings.Fields(line)
        choice := int(rune(split[1][0])) - int('X') + 1

        other := int(rune(split[0][0])) - int('A') + 1
        // fmt.Println(other, choice)
        score += eval(choice, other)

        switch split[1] {
            case "X":
                choice = other - 1
                if choice == 0 {
                    choice = 3
                }                    
            case "Y":
                choice = other
            case "Z":
                choice = other + 1
                if choice == 4 {
                    choice = 1
                }
        }
        fmt.Println(other, choice)
        score2 += eval(choice, other)
    }
    fmt.Println(score)
    fmt.Println(score2)
}

// vim: expandtab tw=99 colorcolumn=100
