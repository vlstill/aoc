package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        buf := make([]rune, 4)
        for i, v := range line {
            buf[i % 4] = v
            same := false
            for j, a := range buf {
                for k, b := range buf {
                    if j != k && a == b {
                        same = true
                    }
                }
            }
            if i >= 3 && !same {
                pt1 += i + 1
                break
            }
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}
