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
type Set[T comparable] map[T]unit

// self looks like value, right? WRONG, you can modify the set, you just can't re-bind it o\
func (self Set[T]) Insert(v T) bool {
    if _, ok := self[v]; !ok {
        self[v] = unit{}  // fuj
        return true
    }
    return false
}

func (self Set[T]) Has(v T) bool {
    _, ok := self[v]
    return ok
}

func (self Set[T]) Delete(v T) {
    delete(self, v)
}

func Intersect[T comparable](a, b Set[T]) (out Set[T]) {
    // better not modify a and b or someone will get nasty surprise…
    out = make(Set[T])
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

func MkSet[T comparable](vals []T) (out Set[T]) {
    out = make(Set[T])
    for _, v := range vals {
        out.Insert(v)
    }
    return
}

func in_all(elves [][]int) int {
    var comm Set[int] = nil
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
