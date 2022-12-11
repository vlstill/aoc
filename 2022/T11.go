package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
)

type Monkey struct {
    Items []int64
    Op func(int64)int64
    TestDivBy int64
    NextTrue int64
    NextFalse int64
    Inspects int64
}

func dump(monkeys []Monkey) {
    activity := []int64{}
    for _, m := range monkeys {
        activity = append(activity, m.Inspects)
    }
    activity = utils.SortDesc(activity)
    fmt.Println(activity[0] * activity[1])
}

func solve(monkeys []Monkey, steps int, divBy int64) {
    modulus := int64(1)
    for _, m := range monkeys {
        modulus *= m.TestDivBy
    }
    for i := 0; i < steps; i++ {
        for i, m := range monkeys {
            monkeys[i].Inspects += int64(len(m.Items))
            items := m.Items
            monkeys[i].Items = []int64{}
            for _, worry := range items {
                newWorry := (m.Op(worry) / divBy) % modulus
                if newWorry % m.TestDivBy == 0 {
                    monkeys[m.NextTrue].Items = append(monkeys[m.NextTrue].Items, newWorry)
                } else {
                    monkeys[m.NextFalse].Items = append(monkeys[m.NextFalse].Items, newWorry)
                }
            }
        }
    }
    dump(monkeys)
}

func main() {
    monkeys := []Monkey{}
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(strings.Replace(strings.Trim(line, " "), "If ", "If", 1), " ")
        if len(split) <= 1 {
            continue
        }
        arg := split[len(split) - 1]
        val, _ := strconv.Atoi(arg)
        last := len(monkeys) - 1
        switch strings.Trim(split[0], ":") {
            case "Monkey":
                monkeys = append(monkeys, Monkey{})
            case "Starting":
                splitC := strings.Split(line, ":")
                arg := splitC[1]
                itemsS := strings.Split(arg, ",")
                items := []int64{}
                for _, str := range itemsS {
                    val, _ := strconv.Atoi(strings.Trim(str, " "))
                    items = append(items, int64(val))
                }
                monkeys[last].Items = items
            case "Operation":
                op := split[len(split) - 2]
                if arg == "old" {
                    if op == "+" {
                        monkeys[last].Op = func(old int64) int64 { return old + old }
                    } else {
                        monkeys[last].Op = func(old int64) int64 { return old * old }
                    }
                } else {
                    if op == "+" {
                        monkeys[last].Op = func(old int64) int64 { return old + int64(val) }
                    } else {
                        monkeys[last].Op = func(old int64) int64 { return old * int64(val) }
                    }
                }
            case "Test":
                monkeys[last].TestDivBy = int64(val)
            case "Iftrue":
                monkeys[last].NextTrue = int64(val)
            case "Iffalse":
                monkeys[last].NextFalse = int64(val)
        }
    }

    solve(append([]Monkey{}, monkeys...), 20, 3)  // COPY!
    solve(monkeys, 10000, 1)
}

// vim: expandtab tw=99 colorcolumn=100
