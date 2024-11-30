package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup

	var n int = 10
	// m := 100

	sayHello := func() {
		for i := 0; i < n ; i ++ {
			defer wg.Done()
			time.Sleep(100 * time.Millisecond)
			fmt.Println("hello")
		}
	}

	sayWorld := func() {
		for i := 0; i < n ; i ++ {
			defer wg.Done()
			// time.Sleep(100 * time.Millisecond)
			fmt.Println("World")
		}
	}

	wg.Add(n)
	go sayHello()
	wg.Add(n)

	go sayWorld()
	wg.Wait()
	// go sayWorld()

}