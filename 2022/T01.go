package main

import (
    "os"
    "fmt"
    "bufio"
    "math"
    "strconv"
    "sort"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    last := 0
    max := 0
    cals := make([]int, 0)
    for scanner.Scan() {
	line := scanner.Text()
	fmt.Println("read:", line)
	val, _ := strconv.Atoi(line)
	if line == "" {
	    max = int(math.Max(float64(last), float64(max)))
	    cals = append(cals, last)
	    last = 0
	} else {
	    last += val
	}
	// split := strings.Split(line, " ")
	// split := strings.Fields(line)
	// fmt.Println("split:", split)
    }
    max = int(math.Max(float64(last), float64(max)))
    cals = append(cals, last)
    
    fmt.Println(max)
    sort.Slice(cals, func(i, j int) bool {
        return cals[i] > cals[j]
	})
    fmt.Println(cals[0] + cals[1] + cals[2])
}
