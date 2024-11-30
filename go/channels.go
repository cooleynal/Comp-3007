package main

import (
	"fmt"
	"sync"
	"time"
)

func Producer(c chan int, wg *sync.WaitGroup, b int) {
	defer wg.Done()
	for i := 0; i < b; i++ {
		c <- i // Produce an item and send it to the channel
		fmt.Printf("Produced: %d\n", i)
		time.Sleep(time.Millisecond * 10) // Simulate work
	}
}

func Consumer(c chan int, wg *sync.WaitGroup, b int) {
	defer wg.Done()
	for i := 0; i < b; i++ {
		item := <-c // Consume an item from the channel
		fmt.Printf("Consumed: %d\n", item)
		time.Sleep(time.Millisecond * 20) // Simulate work
	}
}

func main() {
	b := 5 // Number of items to produce and consume
	c := make(chan int, 5) // Buffer size 5 to allow some data to be buffered
	var wg sync.WaitGroup

	wg.Add(2) // Two goroutines to wait for

	go Producer(c, &wg, b)
	go Consumer(c, &wg, b)

	wg.Wait() // Wait for both producer and consumer to finish
	fmt.Println("Producer and Consumer finished with wait.")
}
