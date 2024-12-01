/*
	Author: Nick Quinn
	Description: Implementing Fibonacci with time and space complexity of O(n)
*/

package main

import "fmt"

func main () {
	// Map that holds the calculated values of fib
	memo := make(map[int]int)
	calculate := [4]int{1, 8, 25, 50}

	for _, n := range calculate {
		fmt.Printf("Fibonacci of %v equals %v\n", n, fib(n, memo))
	}
}

func fib(a int, memo map[int]int) int {
	// If answer has already been calculated, then return value
	if val, ok := memo[a]; ok {
		return val
	}

	// If value is base case (1 or 2), then return value
	if a <= 2 {
		return 1
	}

	// Otherwise, calculate fibonacci value, and return
	memo[a] = fib(a - 1, memo) + fib(a -2, memo)
	return memo[a]

}