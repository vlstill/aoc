package main

import (
//    "bufio"
    "fmt"
//    "os"
    "strings"
    "strconv"
    "aoc/utils"
)

func use(_ interface{}) {
    if 4 == 2 {
        _ = utils.Min(4, 2)
        _ = strings.Split("", "")
        _, _ = strconv.Atoi("42")
    }
}

type Monkey struct {
    Items []int64
    Op func(int64)int64
    TestDivBy int64
    NextTrue int64
    NextFalse int64
    Inspects int64
}

func dump(monkeys []Monkey) {
    fmt.Println(monkeys)
    activity := []int64{}
    for _, m := range monkeys {
        activity = append(activity, m.Inspects)
    }
    activity = utils.SortDesc(activity)
    fmt.Println(activity[0] * activity[1])
}

func main() {
    monkeys := []Monkey{
        Monkey{[]int64{79, 98}, func(x int64) int64 { return x * 19; }, 23, 2, 3, 0},
        Monkey{[]int64{54, 65, 75, 74}, func(x int64) int64 { return x + 6; }, 19, 2, 0, 0},
        Monkey{[]int64{79, 60, 97}, func(x int64) int64 { return x * x; }, 13, 1, 3, 0},
        Monkey{[]int64{74}, func(x int64) int64 { return x + 3; }, 17, 0, 1, 0}}

    modulus := int64(1)
    for _, m := range monkeys {
        modulus *= m.TestDivBy
    }
    fmt.Println(monkeys)
    for i := 0; i < 10000; i++ {
        for i, m := range monkeys {
            monkeys[i].Inspects += int64(len(m.Items))
            items := m.Items
            monkeys[i].Items = []int64{}
            for _, worry := range items {
                newWorry := m.Op(worry) % modulus
                if newWorry % m.TestDivBy == 0 {
//                    fmt.Println(worry, "@", newWorry, "goto", m.NextTrue)
                    monkeys[m.NextTrue].Items = append(monkeys[m.NextTrue].Items, newWorry)
                } else {
//                    fmt.Println(worry, "@", newWorry, "goto", m.NextFalse)
                    monkeys[m.NextFalse].Items = append(monkeys[m.NextFalse].Items, newWorry)
                }
            }
//            fmt.Println(monkeys)
        }
        if i == 19 || i == 0 {
            dump(monkeys)
        }
    }
    dump(monkeys)
//    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
