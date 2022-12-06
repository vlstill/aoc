package main

import (
    "bufio"
    "fmt"
    "os"
)

func scanline(line string, cnt int) int {
    buf := make([]rune, cnt)
    for i, v := range line {
        buf[i % cnt] = v
        same := false
        for j, a := range buf {
            for k, b := range buf {
                if j != k && a == b {
                    same = true
                }
            }
        }
        if i >= cnt - 1 && !same {
            return i + 1
        }
    }
    return 0
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    for scanner.Scan() {
        line := scanner.Text()
        pt1 += scanline(line, 4)
        pt2 += scanline(line, 14)
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}
