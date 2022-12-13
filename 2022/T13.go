package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
    "sort"
)

func use(_ interface{}) {
    if 4 == 2 {
        _ = utils.Min(4, 2)
        _ = strings.Split("", "")
        _, _ = strconv.Atoi("42")
    }
}

type List struct {
    IsVal bool
    Val int
    List []List
}

func parse(str string) List {
//    fmt.Println("parse", str, len(str))
    if str[0] == '[' {
        depth := 0
        out := List{false, 0, []List{}}
        start := 1

        for i, v := range str {
            if v == '[' {
                depth++
            } else if v == ']' {
                depth--
                if depth == 0 {
                    if start != i {
                        out.List = append(out.List, parse(str[start:i]))
                    }
                    break
                }
            }
            if depth == 1 && v == ',' {
                out.List = append(out.List, parse(str[start:i]))
                start = i + 1
            }
        }
        return out
    } else {
        v, _ := strconv.Atoi(str)
        return List{true, v, nil}
    }
}

func cmp(left, right List) int {
    fmt.Println("cmp", left, right)
    if left.IsVal && right.IsVal {
        return left.Val - right.Val
    }
    if left.IsVal {
        left = List{false, 0, []List{List{true, left.Val, nil}}}
    }
    if right.IsVal {
        right = List{false, 0, []List{List{true, right.Val, nil}}}
    }
    fmt.Println("cmp-lst", left, right)
    for i := 0; i < utils.Max(len(left.List), len(right.List)); i++ {
        if len(left.List) <= i {
            return -1
        }
        if len(right.List) <= i {
            return 1
        }
        if v := cmp(left.List[i], right.List[i]); v < 0 {
            return v
        } else if v > 0 {
            return v
        }
    }
    return 0
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 1
    idx := 0
    div0 := parse("[[2]]")
    div1 := parse("[[6]]")
    packets := []List{div0, div1}
    for scanner.Scan() {
        left := parse(scanner.Text())
        scanner.Scan()
        right := parse(scanner.Text())
        packets = append(packets, left, right)
        idx++
        if cmp(left, right) < 0 {
            pt1 += idx
        }
        if !scanner.Scan() {  // sep
            break
        }
    }
    fmt.Println(pt1)

    sort.Slice(packets, func(i, j int) bool { return cmp(packets[i], packets[j]) < 0 })
    for i, v := range packets {
        if cmp(div0, v) == 0 || cmp(div1, v) == 0 {
            pt2 *= i + 1
        }
    }
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
