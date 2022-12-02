package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func eval(choice, other int) (score int) {
    score = choice + 1
    if choice == other {
        score += 3
    } else if (choice - other == 1 || other - choice == 2) {
        score += 6
    }
    return
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    score := 0
    score2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Fields(line)

        choice := int(split[1][0]) - int('X')
        other := int(split[0][0]) - int('A')
        score += eval(choice, other)

        switch split[1] {
            case "X":
                choice = (other + 2) % 3
            case "Y":
                choice = other
            case "Z":
                choice = (other + 1) % 3
        }
        score2 += eval(choice, other)
    }
    fmt.Println(score)
    fmt.Println(score2)
}

// vim: expandtab tw=99 colorcolumn=100
