package main

import (
    "bufio"
    "fmt"
    "os"
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

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    hx, hy := 0, 0
    tx, ty := 0, 0
    seen := make(map[int]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        use(split)
        cnt, _ := strconv.Atoi(split[1])
        fmt.Println(split[0], cnt)
        for i := 0; i < cnt; i++ {
            switch split[0] {
                case "R":
                    hx++;
                    if hx > tx + 1 {
                        tx++
                        if hy != ty {
                            if hy > ty {
                                ty++
                            } else {
                                ty--
                            }
                        }
                    }
                case "L":
                    hx--;
                    if hx < tx - 1 {
                        tx--
                        if hy != ty {
                            if hy > ty {
                                ty++
                            } else {
                                ty--
                            }
                        }
                    }

                case "U":
                    hy++;
                    if hy > ty + 1 {
                        ty++
                        if hx != tx {
                            if hx > tx {
                                tx++
                            } else {
                                tx--
                            }
                        }
                    }
                case "D":
                    hy--;
                    if hy < ty - 1 {
                        ty--
                        if hx != tx {
                            if hx > tx {
                                tx++
                            } else {
                                tx--
                            }
                        }
                    }
                default:
                    fmt.Println("invalid")
            }
            seen[1000 * tx + ty] = 1
            fmt.Printf("(%d, %d) (%d, %d)\n", hx, hy, tx, ty)
        }
    }
    pt1 = len(seen)
    fmt.Println(pt1)
    fmt.Println(pt2)
}

// vim: expandtab tw=99 colorcolumn=100
