package main

// import (
// 	"bytes"
// 	"fmt"
// 	"os"
// )

// func main() {
// 	var stdoutBuff bytes.Buffer
// 	defer stdoutBuff.WriteTo(os.Stdout)
// 	intStream := make(chan int,1)

// 	go func() {
// 		defer close(intStream)

// 		defer fmt.Fprintln(&stdoutBuff, "Producer Done.")

// 		for i := 0; i < 5; i++ {
// 			fmt.Fprintf(&stdoutBuff, "Sending: %d\n", i)
// 			intStream <- i
// 		}
// 	}()

// 	for integer := range intStream {
// 		fmt.Fprintf(&stdoutBuff, "Received %v.\n", integer)
// 	}
// }

// package main

import (
	"fmt"
	"time"
)

// func main() {
// 	intStream := make(chan int, 3)

// 	// go func() {
// 	// 	for value := range intStream {
// 	// 		fmt.Printf("Received: %d\n", value)
// 	// 	}
// 	// }()

// 	// for i := 0; i < 20; i++ {
// 	// 	fmt.Printf("Attempting to send: %d\n", i)
// 	// 	intStream <- i
// 	// }

// 	intStream <- 1
// 	intStream <- 2
// 	intStream <- 3
// 	fmt.Printf("output: %d\n", <- intStream)
// 	fmt.Printf("output: %d\n", <- intStream)
// 	fmt.Printf("output: %d\n", <- intStream)

// 	// close(intStream)
// }



func main() {
	intStream := make(chan int, 2)

	go func() {
		for value := range intStream {
			fmt.Printf("Received: %d\n", value)
			time.Sleep(500 * time.Millisecond)
		}
	}()

	for i := 0; i < 101; i++ {
		fmt.Printf("Attempting to send: %d\n", i)
		intStream <- i
	}

	close(intStream)
}

