package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 0
    path := []string{}
    dirsz := make(map[string]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        if (split[0] == "$" && split[1] == "cd") {
            fmt.Println(dirsz)
            if split[2] == ".." {
                path = path[0:len(path)-1]
            } else if split[2] == "/" {
                path = []string{};
            } else {
                path = append(path, split[2])
            }
            fmt.Println(path)
        } else if split[0] == "$" && split[1] == "ls" {
            continue
        } else if split[0] != "dir" {
            sz, _ := strconv.Atoi(split[0])
            fmt.Println(sz)
            for i := 0; i <= len(path); i++ {
                dirsz[strings.Join(path[0:i], "/")] += sz
            }
        }        
    }
    for _, v := range dirsz {
        if (v < 100000) {
            pt1 += v
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}
