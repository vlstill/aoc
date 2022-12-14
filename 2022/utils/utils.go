package utils

import (
    "golang.org/x/exp/constraints"
    "sort"
    "fmt"
)

type OrdNum interface {
    constraints.Integer | constraints.Float
}

type OrdSignedNum interface {
    constraints.Signed | constraints.Float
}

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

func Abs[T OrdNum](a T) T {
    if a < T(0) {
        return -a;
    }
    return a;    
}

func Signum[T OrdSignedNum](x T) T {
    if x > T(0) {
        return T(1)
    } else if x < T(0) {
        return T(-1)
    }
    return T(0)
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

type Point struct {
    X int
    Y int
}

type Map[T any] [][]T

func (self Map[T]) Xd() int {
    return len(self[0])
}

func (self Map[T]) Yd() int {
    return len(self)
}

func (self Map[T]) At(p Point) T {
    return self[p.Y][p.X]
}

func (self Map[T]) Set(p Point, val T) {
    self[p.Y][p.X] = val
}

func (self Map[T]) In(p Point) bool {
    return p.X >= 0 && p.Y >= 0 && p.X < self.Xd() && p.Y < self.Yd()
}

func (self Map[T]) FillLine(from, to Point, val T) {
    dx := Signum(to.X - from.X)
    dy := Signum(to.Y - from.Y)
//    fmt.Println("FillLine", from, "->", to, "=", val, ":", dx, dy)
    for from.X != to.X || from.Y != to.Y {
//        fmt.Println("FillLine", from, "=", val)
        self.Set(from, val)
        from = Point{from.X + dx, from.Y + dy}
    }
    self.Set(from, val)
}

func MakeMap[T any](xdim, ydim int, fill T) Map[T] {
    map_ := make([][]T, ydim)
    for i, _ := range map_ {
        map_[i] = make([]T, xdim)
        for j, _ := range map_[i] {
            map_[i][j] = fill
        }
    }
    return map_
}

func (self Map[T]) DumpSlice(from, to Point) {
    for y := from.Y; y <= to.Y; y++ {
        for x := from.X; x <= to.X; x++ {
            fmt.Print(self.At(Point{x, y}))
        }
        fmt.Println()
    }
}

func (self Map[T]) Clone() Map[T] {
    out := Map[T](make([][]T, len(self)))
    for i, v := range self {
        out[i] = append([]T{}, v...)
    }
    return out
}
