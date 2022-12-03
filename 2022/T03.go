package main

import (
    "bufio"
    "fmt"
    "os"
//    "strings"
)

func decode(str string) (out []int) {
    out = make([]int, len(str))
    for i := 0; i < len(str); i++ {
        if ('a' <= str[i] && str[i] <= 'z') {
            out[i] = int(str[i]) - 'a' + 1
        } else {
            out[i] = int(str[i]) - 'A' + 27
        }
    }
    return
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    score := 0
    score2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        first := decode(line[0:len(line)/2])
        second := decode(line[len(line)/2:])
        fmt.Println(first, second)

        found := false
        for i := 0; !found && i < len(first); i++ {
            for j := 0; !found && j < len(second); j++ {
                if first[i] == second[j] {
                    score += first[i]
                    found = true
                }
            }
        }


        score += 0
        score2 += 0
    }
    fmt.Println(score)
    fmt.Println(score2)
}

// vim: expandtab tw=99 colorcolumn=100
