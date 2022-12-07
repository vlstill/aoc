package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
    "aoc/utils"
    "path/filepath"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    pt1 := 0
    pt2 := 70000000
    path := []string{}
    dirsz := make(map[string]int)
    for scanner.Scan() {
        line := scanner.Text()
        split := strings.Split(line, " ")
        if (split[0] == "$" && split[1] == "cd") {
            if split[2] == ".." {
                path = path[0:len(path)-1]
            } else if split[2] == "/" {
                path = []string{};
            } else {
                path = append(path, split[2])
            }
        } else if split[0] != "dir" {
            sz, _ := strconv.Atoi(split[0])
            for i := 0; i <= len(path); i++ {
                dirsz[filepath.Join(path[0:i]...)] += sz
            }
        }        
    }
    total := dirsz[""]
    pt2 = total
    need := 30000000 - (70000000 - total)
    for _, v := range dirsz {
        if (v < 100000) {
            pt1 += v
        }
        if v > need {
            pt2 = utils.Min(pt2, v)
        }
    }
    fmt.Println(pt1)
    fmt.Println(pt2)
}
