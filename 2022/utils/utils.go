package utils

import (
    "golang.org/x/exp/constraints"
    "sort"
)

func Min[T constraints.Ordered](a, b T) T {
    if a < b {
	return a
    }
    return b
}

func Max[T constraints.Ordered](a, b T) T {
    if a > b {
	return a
    }
    return b
}

func Abs(a int) int {
    if a < 0 {
        return -a;
    }
    return a;    
}

func Signum(x int) int {
    if x > 0 {
        return 1
    } else if x < 0 {
        return -1
    }
    return 0
}

type MinOrdered[T constraints.Ordered] []T

func (x MinOrdered[T]) Len() int { return len(x) }
func (x MinOrdered[T]) Less(i, j int) bool { return x[i] < x[j] }
func (x MinOrdered[T]) Swap(i, j int) { x[j], x[i] = x[i], x[j] }

type MaxOrdered[T constraints.Ordered] []T

func (x MaxOrdered[T]) Len() int { return len(x) }
func (x MaxOrdered[T]) Less(i, j int) bool { return x[i] > x[j] }
func (x MaxOrdered[T]) Swap(i, j int) { x[j], x[i] = x[i], x[j] }

func Sort[T constraints.Ordered](arr []T) []T {
    out := make([]T, len(arr))
    copy(out, arr)
    sort.Sort(MinOrdered[T](out));
    return out;
}

func SortDesc[T constraints.Ordered](arr []T) []T {
    out := make([]T, len(arr))
    copy(out, arr)
    sort.Sort(MaxOrdered[T](out));
    return out;
}

type Unit struct{}
type Set[T comparable] map[T]Unit

// self looks like value, right? WRONG, you can modify the set, you just can't re-bind it o\
func (self Set[T]) Insert(v T) bool {
    if _, ok := self[v]; !ok {
        self[v] = Unit{}  // fuj
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
