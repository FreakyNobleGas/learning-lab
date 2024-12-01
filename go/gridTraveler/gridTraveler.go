/*
	Author: Nick Quinn
	Description: Implementing solution for GridTraveler problem
	Problem Statement: If presented with a m by n grid, how many ways can you reach
	                   the bottom right of the grid from starting at the top left. Additionally,
					   you can only move right or downwards.
	Complexity: With brute force, height of tree is n + m, and since this is a binary tree, we can say the time complexity is O(2^n+m) and space
	            complexity is O(n+m). With memoized strategy, time complexity becomes O(n*m), because we eliminate many duplicate subtrees and 
				space complexity stays at O(n+m)
*/

package main

import "fmt"

type grid struct {m, n int}

func main () {
	// Map that holds the calculated values of fib
	memo := make(map[grid]int)
	calculate := []grid{{1,1}, {2,3}, {3,2}, {3,3}, {18,18}}

	for _, g := range calculate {
		fmt.Printf("Possible moves for %v,%v grid is %v\n", g.m, g.n, gridTraveler(g, memo))
	}
}

func gridTraveler(g grid, memo map[grid]int) int {
	// Check if grid has already been calcuated
	if g, has_key := memo[g]; has_key {
		return g
	}

	// Base cases
	if g.m == 1 && g.n == 1 {
		return 1
	}
	if g.m == 0 || g.n == 0 {
		return 0
	}

	// Store new grid value and return
	memo[g] = gridTraveler(grid{g.m - 1, g.n}, memo) + gridTraveler(grid{g.m, g.n - 1}, memo) 
	return memo[g]
}