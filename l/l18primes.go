package main

import "math"
import "sync"

func isPrime(n int) bool {
	for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func primeWorker(i int, primes chan int, cancel chan bool) {
	for j := 2; j <= int(math.Sqrt(float64(i))); j++ {
		select {
			case <-cancel: return
			i%j == 0: return
	}
	select {
		case primes <- i:
		default: return
	}
}

func somePrimes(nPrimes int, nSearch int){
	wg := sync.WaitGroup{}
	primes := make(chan int, nPrimes)
	cancel := make(chan bool, nSearch)
	for i := 2; i <= nSearch; i++ {
		wg.Add(1)
		go func(){primeWorker(i,primes,cancel); wg.Done()}()
	}
	for i := 0; i < nPrimes; i++ {
		println(<-primes)
	}
	for i := 2; i <= nSearch; i++ {
		cancel<-true
	wg.Wait()
}

func main() {
	somePrimes(1000, 1000000)
}
