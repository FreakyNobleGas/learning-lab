package main

import (
	"fmt"
	"unsafe"
)

func main() {
	// Print to stdout
	fmt.Println("Hello, World!")

	// Basic variable declaration
	var s string
	s = "Hello World!"
	fmt.Printf("s is: '%s'\n", s)

	// Assign value to variable after declaring
	var x int
	x = 3
	fmt.Printf("x is: '%v'\n", x)

	// Infer type by using := without declaring
	y := 4
	fmt.Printf("y is: '%v'\n", y)

	// Declare multiple variables
	var a, b, c string
	a, b, c = "hi", "bye", "hello"
	fmt.Printf("%s %s %s\n", a, b, c)

	// Bool - Note: Bool defaults to false if not declared
	var d = false
	fmt.Printf("d is: '%v'\n", d)

	// Bool Inferred
	e := true
	fmt.Printf("size of bool is: %d\n", unsafe.Sizeof(e))

	// Example of function call with multiple values returned
	f, g := multipleReturn()
	h, i := multipleReturn2()
	fmt.Printf("f: %d, g: %d h: %d, i: %d\n", f, g, h, i)
}

// Example 1: Return multiple values
func multipleReturn() (int, int) {
	return 1, 2
}

// Example 2: Return multiple values
func multipleReturn2() (a int, b int) {
	a = 3
	b = 4
	return
}