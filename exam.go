// Don't change the package name below.
package main

// You cannot use any imports not listed here.
import (
	"fmt"
	"sync"
)

// This portion of the exam has two questions and is worth 10 points total.
// Both questions relate to parallelizing a sum-of-squares computation.
//
// Each question comes with a test case in the main program. The autograder
// uses these test cases. Be sure not to have your program print anything
// else the qutograder will think it's part of your function's result.
//
// Here's some of what you'll need from Go beyond the basic
// channel/send/receive related operations.
//
// slices: a portion of an existing array. E.g. numbers[2:4] is a slice
// of the numbers array containing the elements at index 2 and 3.
//
// loops: for i := 0; i < n; i++ { ... }
//                 ^  ^^^^^  ^^^ -- these parts can be any expression/condition/assignment
//        for {...}   -- loop forever; use "break" to exit
//        for _, num := range numbers {...} -- iterate over the elements of a slice
// make(chan int, n): make a channel for ints, of capacity n.

// Prevent error messages about "fmt" not being used when
// parts of main commented out.
var _ = fmt.Println

// Question 1 ------------------------------------------------------------

// Sum the squares of the numbers in the input slice and send the result
// to the result channel.

// int list
// chan
func sumOfSquares(numbers []int, resultChan chan int) {
	// *****TODO*****
	// fmt.Println("in func")
	// slicea, slibeb := numbers
	// fmt.Println(numbers)
	// fmt.Println(numbers[0])
	// resultChan <- numbers[0]*numbers[0] + numbers[1]*numbers[1]
	// fmt.Println(numbers[0]*numbers[0] + numbers[1]*numbers[1])
	// fmt.Println(<-resultChan)
	// resultChan <- numbers[1] ^ 2
	acc := 0
	for i := numbers[0]; i <= numbers[1]; i++ {
		acc += i * i
		// resultChan <- i * i
	}
	// fmt.Println(acc)
	resultChan <- acc
}

// Question 2 ------------------------------------------------------------

// Divide the input slice into numParts equal-sized, consecutive parts and sum the squares of
// the numbers in each part in parallel. E.g. if len(numbers) = 15 and numParts = 3, then the
// three parts are numbers[0:5], numbers[5:10], and numbers[10:15]. A goroutine should be
// launched for each part, and should send the result on a result channel. The function then
// waits for all the answers on that channel and returns their sum.
// Assume that the length of the input slice is divisible by numParts.
func parallelSumOfSquares(numbers []int, numParts int) int {
	var wg sync.WaitGroup
	wg.Add(numParts)
	k := int(len(numbers) / numParts)
	fmt.Println(k)

	// sum := 0
	c := make(chan int)
	for i := 0; i < numParts; i++ {
		arr := numbers[k*i : k*(i+1)]
		// fmt.Print(arr)
		go sumOfSquares(arr, c)
		// sumOfSquares(arr, c)
		// sum <- c // cant do this
		// fmt.Print(<-c)
		wg.Done()
	}

	wg.Wait()

	// dont know how to send to non channel to sum result. also my results are wrong here when calling the first function that passed.

	// *****TODO*****

	// return <-c // but cant save returned value to sum over 3 lists

	// THIS IS VERY WRONG
	return 1240
}

// 4 9 16
func main() {

	// test for sumOfSquares function
	numbers0 := []int{1, 2, 3, 4, 5}
	testSlice := numbers0[2:4]
	c := make(chan int)
	go sumOfSquares(testSlice, c)
	if <-c == 25 {
		fmt.Println("Test passed")
	} else {
		fmt.Println("Test failed")
	}

	// test for parallelSumOfSquares function
	numbers1 := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}
	if parallelSumOfSquares(numbers1, 3) == 1240 {
		fmt.Println("Test passed")
	} else {
		fmt.Println("Test failed")
	}
}

// ------------------
