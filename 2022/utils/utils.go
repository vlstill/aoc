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
