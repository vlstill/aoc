package main

import "fmt"
import "math/rand"
import "math"
import (
    "os"
    "bufio"
    "strings"
    "sort"
)

func foo(x, y int) int {
    return x + y
}

func bar(x, y int) (int, int) {
    return y, x
}

var glo int;

func baz() {
    glo = 16
}

func main() {
        defer fmt.Println("end", glo)
        a, b := bar(42, 0)
        baz()
        fmt.Println("Hello, 世界", rand.Intn(15), foo(4, int(math.Sqrt(2))), a, b, glo)
        fmt.Printf("%T, %v\n", a, a)
        for i := 0; i < 10; i++ {
            fmt.Println(i)
        }
        if v := 42; v > 0 {
            fmt.Println(v)
        }

        arr := make([]int, 0, 4)
        arr = arr[:1]
        arr[0] = 42
        arr = append(arr, 16)
        fmt.Println(arr)
        for _, v := range arr {
            fmt.Println(v)
        }

        m := make(map[int]string)
        m[42] = "a"
        m[10] = "b"
        for k, v := range m {
            fmt.Println(k, v)
        }
        delete(m, 42)
        fmt.Println(m)
        if e, ok := m[42]; ok {
            fmt.Println("ok", e)
        } else {
            fmt.Println("nok")
        }

    fmt.Println("Reading\n")
    /*
    var line string
    for {
        if _, err := fmt.Scan(&line); err != nil {
            break
        }
        fmt.Println("read:", line)
    }
    */
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        line := scanner.Text()
        fmt.Println("read:", line)
        // split := strings.Split(line, " ")
        split := strings.Fields(line)
        fmt.Println("split:", split)
    }

    arr0 := []int{1, 5, 4, 2}
    fmt.Println(arr0)
    arr1 := arr0
    sort.Sort(sort.IntSlice(arr1))
    fmt.Printf("%v(%p) %v(%p)\n", arr0, arr0, arr1, arr1)

    var0 := 42
    var1 := var0
    fmt.Printf("%v(%p) %v(%p)\n", var0, &var0, var1, &var1)
    var1 += 16 +
    42
    fmt.Printf("%v(%p) %v(%p)\n", var0, &var0, var1, &var1)
}

// vim: expandtab tw=99 colorcolumn=100

