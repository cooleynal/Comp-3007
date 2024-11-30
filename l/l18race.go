package main

import (
	"sync"
)

type counter struct {
	count int
	lock  chan struct{}
}

func increment(c *counter) {
	x := <-c.lock  // can omit x
	(*c).count++
	(*c).lock <- x
}

func race() {
	var wg sync.WaitGroup
	counter := &counter{lock: make(chan struct{}, 1)}
	counter.count = 0
	counter.lock <- struct{}{}
	for i := 0; i < 1000000; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			increment(counter)
		}()
	}
	wg.Wait()
	println(counter.count)
}

func main() { race() }
