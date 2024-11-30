package main

import (
	"math/rand"
	"sync"
	"time"
)

func factorial(n int) int {
	if n == 0 {
		return 1
	}
	return n * factorial(n-1)
}

func factorialMain(){
	var rands, results [100]int
	n := 30
	max := 30
	for i := 0; i < n; i++ {
		rands[i] = rand.Intn(max)
	}
	for i:=0; i<n; i++ {
		go func () {results[i] = factorial(rands[i])}()
	}
	time.Sleep(1 * time.Second)
	for i:=0; i<n; i++ {
		println(rands[i], ": ", results[i])
	}
}

func producerConsumerMain() {
	var wg sync.WaitGroup
	var c = make(chan int, 0)
	for i := 0; i < 10; i++ {
		go func() {c<- i}()
	}
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func() {println(<-c); wg.Done()}()
	}
	wg.Wait()
}

func race1() {
	var count int
	var counters sync.WaitGroup
	for i := 0; i < 100000; i++ {
		counters.Add(1)
		go func() { count++; counters.Done() }()
	}
	counters.Wait()
	println(count)
}

type U struct{}

var u = U{}

type counter struct {
	lock chan U
	value *int
}

// func main() {
// 	race1()
// }


















func count() {
	var count int
	for i := 0; i < 100000; i++ {
		go func() { count++ }()
	}
	time.Sleep(time.Second)
	println(count)
}

func countMain() {
	count()
}
