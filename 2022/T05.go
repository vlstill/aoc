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

func play(stacks0 [][]byte, instrs [][]int, move_more bool) string {
    stacks := make([][]byte, len(stacks0))
    for i, st := range stacks0 {
        stacks[i] = append([]byte{}, st...)
    }
    // fmt.Println(stacks)
    for _, instr := range instrs {
        cnt := instr[0]
        from := instr[1]
        to := instr[2]
        // fmt.Println(cnt, from, to)
        if move_more {
            move(stacks, from, to, cnt)
        } else {
            for i := 0; i < cnt; i++ {
                move(stacks, from, to, 1)
            }
        }
        // fmt.Println(stacks)
    }
    out := make([]byte, 0, len(stacks))
    for _, st := range stacks {
        out = append(out, st[len(st) - 1])
    }
    return string(out)
}

//    v, _ := strconv.Atoi(split[1])
func main() {
    scanner := bufio.NewScanner(os.Stdin)
    stacks := make([][]byte, 0)
    instrs := make([][]int, 0)
    state := 0
    for scanner.Scan() {
        line := scanner.Text()
        // fmt.Println(line)
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
                    // fmt.Println(stacks)
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
            instrs = append(instrs, []int{cnt, from, to})
        }        
    }
    fmt.Println(play(stacks, instrs, false))
    fmt.Println(play(stacks, instrs, true))
}

// vim: expandtab tw=99 colorcolumn=100
