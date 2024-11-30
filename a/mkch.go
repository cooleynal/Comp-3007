package main

import "fmt"
var stringStream = make(chan string, 1)

func sendMessage() chan string {
	// stringStream := make(chan string, 1)

	go func() {
		stringStream <- "11111111111111"
		stringStream <- "222222222222222"
		stringStream <- "3333333333333"
	}()

	go func() {
		stringStream <- "44444444"
		stringStream <- "555555555555"
		stringStream <- "66666666"
	}()

	return stringStream
}

func main() {
	messageChannel := sendMessage()
	fmt.Println(<-messageChannel)
	fmt.Println(<-messageChannel)
	fmt.Println(<-messageChannel)

	fmt.Println(<-messageChannel)

	// fmt.Println(<-sendMessage())
}
