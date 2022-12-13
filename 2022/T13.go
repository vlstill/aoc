package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "aoc/utils"
    "sort"
)

type NestedList interface {
    cmp(NestedList) int
}

type Val int
type List []NestedList

func parse(str string) NestedList {
    if str[0] == '[' {
        depth := 0
        out := List{}
        start := 1

        for i, v := range str {
            if v == '[' {
                depth++
            } else if v == ']' {
                depth--
                if depth == 0 {
                    if start != i {
                        out = append(out, parse(str[start:i]))
                    }
                    break
                }
            }
            if depth == 1 && v == ',' {
                out = append(out, parse(str[start:i]))
                start = i + 1
            }
        }
        return out
    } else {
        v, _ := strconv.Atoi(str)
        return Val(v)
    }
}

func (left Val) cmp(right NestedList) int {
    if rightVal, ok := right.(Val); ok {
        return int(left) - int(rightVal)
    }
    if rightList, ok := right.(List); ok {
        leftList := List{left}
        return leftList.cmp(rightList)
    }
    panic("invalid NestedList")
}

func (left List) cmp(right NestedList) int {
    if rightVal, ok := right.(Val); ok {
        right = List{rightVal}
    }
    if rightList, ok := right.(List); ok {
        for i := 0; i < utils.Max(len(left), len(rightList)); i++ {
            if len(left) <= i {
                return -1
            }
            if len(rightList) <= i {
                return 1
            }
            if v := left[i].cmp(rightList[i]); v < 0 {
                return v
            } else if v > 0 {
                return v
            }
        }
        return 0
    }
    panic("invalid NestedList")
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 1
    idx := 0
    div0 := parse("[[2]]")
    div1 := parse("[[6]]")
    packets := []NestedList{div0, div1}
    for scanner.Scan() {
        left := parse(scanner.Text())
        scanner.Scan()
        right := parse(scanner.Text())
        packets = append(packets, left, right)
        idx++
        if left.cmp(right) < 0 {
            pt1 += idx
        }
        if !scanner.Scan() {  // sep
            break
        }
    }
    fmt.Println(pt1)

    sort.Slice(packets, func(i, j int) bool { return packets[i].cmp(packets[j]) < 0 })
    for i, v := range packets {
        if div0.cmp(v) == 0 || div1.cmp(v) == 0 {
            pt2 *= i + 1
        }
    }
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
