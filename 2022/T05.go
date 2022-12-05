package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

func move(stack [][]byte, from, to int, cnt int) {
    idx := len(stack[from]) - cnt
    stack[to] = append(stack[to], stack[from][idx:]...)
    stack[from] = stack[from][0:idx]
}

//    v, _ := strconv.Atoi(split[1])
func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := make([]byte, 0)
    pt2 := 0
    stacks := make([][]byte, 0)
    state := 0
    for scanner.Scan() {
        line := scanner.Text()
        fmt.Println(line)
        if state == 0 {
            if line[1] == '1' {
                state = 1
                continue
            }
            for i := 1; i < len(line); i += 4 {
                pos := i / 4
                if line[i] != ' ' {
                    for len(stacks) <= pos {
                        stacks = append(stacks, make([]byte, 0))
                    }
                    stacks[pos] = append(stacks[pos], line[i])
                    fmt.Println(stacks)
                }
            }
        }
        if state == 1 && len(line) == 0 {
            state = 2
            for _, st := range stacks {
                for j := 0; j < len(st) / 2; j++ {
                    st[j], st[len(st) - j - 1] = st[len(st) - j - 1], st[j]
                }
            }
            continue
        }
        if state == 2 {
            split := strings.Split(line, " ")
            cnt, _ := strconv.Atoi(split[1])
            from, _ := strconv.Atoi(split[3])
            to, _ := strconv.Atoi(split[5])
            from--
            to--
            fmt.Println(cnt, from, to)
            move(stacks, from, to, cnt)
            fmt.Println(stacks)
        }        
    }
    for _, st := range stacks {
        pt1 = append(pt1, st[len(st) - 1])
    }
    fmt.Println(string(pt1))
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
