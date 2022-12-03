package main

import (
    "bufio"
    "fmt"
    "os"
)

func decode(str string) (out []int) {
    out = make([]int, len(str))
    for i, v := range str {
        if ('a' <= v && v <= 'z') {
            out[i] = int(v) - 'a' + 1
        } else {
            out[i] = int(v) - 'A' + 27
        }
    }
    return
}

type unit struct{}
type Set map[int]unit

// self looks like value, right? WRONG, you can modify the set, you just can't re-bind it o\
func (self Set) Insert(v int) bool {
    if _, ok := self[v]; !ok {
        self[v] = unit{}  // fuj
        return true
    }
    return false
}

func (self Set) Has(v int) bool {
    _, ok := self[v]
    return ok
}

func (self Set) Delete(v int) {
    delete(self, v)
}

func Intersect(a, b Set) (out Set) {
    // better not modify a and b or someone will get nasty surprise…
    out = make(Set)
    if len(a) > len(b) {
        a, b = b, a
    }
    for k, _ := range a {
        if _, ok := b[k]; ok {
            out.Insert(k)
        }
    }
    return
}

func MkSet(vals []int) (out Set) {
    out = make(Set)
    for _, v := range vals {
        out.Insert(v)
    }
    return
}

func in_all(elves [][]int) int {
    var comm Set = nil
    for _, bag := range elves {
        if comm == nil {
            comm = MkSet(bag)
        } else {
            comm = Intersect(comm, MkSet(bag))
        }
    }
    for k, _ := range comm {  // fuj
        return k
    }
    return 0
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    group := make([][]int, 0, 3)
    for scanner.Scan() {
        line := scanner.Text()
        all := decode(line)
        first := decode(line[0:len(line)/2])
        second := decode(line[len(line)/2:])

        pt1 += in_all([][]int{first, second})

        group = append(group, all)
        if len(group) == 3 {
            pt2 += in_all(group)
            group = make([][]int, 0, 3)
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
