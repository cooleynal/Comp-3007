// Don't change the package name below nor the name of this file.
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

//
// The file is divided into two parts, the first for A11 and the second
// for A12. The main function for both is at the bottom of the file.
// Fill in the TODOs. Most of the code is supplied. The A12 section (at least)
// will be manually graded with lots of partial credit even if the program
// doesn't run.

// Assignment 11 section.
// This is a simplification of the assignment framework. You'll implement the
// calculator from class as an Interpreter. No lexer, and parsing is
// trivial.
type Parser interface {
	Parse(string) Ast
}

type Ast interface {
	Data() string
	Parts() []Ast
}

type Value interface {
	stringifyValue() string
}

type Evaluator interface {
	Eval(Ast) Value
}

// As before, but no Lexer, and the parser takes a string argument
// instead of a token array slice.
type Interpreter struct {
	Ast    Ast
	Parser Parser
	Evaler Evaluator
}

func Run(i Interpreter, str string) {
	fmt.Printf("%v\n", i.Evaler.Eval(i.Parser.Parse(str)))
}

func Repl(i Interpreter) {
	for {
		reader := bufio.NewReader(os.Stdin)
		fmt.Print("\"repl\" >  ")
		str, err := reader.ReadString('\n')
		if err != nil {
			panic("Repl")
		}
		Run(i, str)
	}
}

// An Ast here is just a string representing an input to the
// calculator (operation or number).
type calcAst string

func (c calcAst) Data() string {
	return string(c)
}

func (c calcAst) Parts() []Ast {
	return []Ast{}
}

type calcParser struct{}

func (c calcParser) Parse(str string) Ast {
	result := calcAst(strings.Trim(str, " \n\r\t"))
	return result
}

// The evaluator is a stack-based calculator. Unlike the Scheme
// evaluator, it has state. The evaluator modifies the stack.
// Pointers are used but you can ignore them and just used the provided
// operations (push etc)
type calcEvaluator struct {
	stack *[]float64
}

func push(c calcEvaluator, v float64) {
	*(c.stack) = append(*(c.stack), v)
}

func pop(c calcEvaluator) float64 {
	n := len(*(c.stack))
	v := (*(c.stack))[max(0, n-1)]
	*(c.stack) = (*(c.stack))[:max(0, n-1)]
	return v
}

func empty(c calcEvaluator) bool {
	return len(*(c.stack)) == 0
}

func calcEvaluatorNew() calcEvaluator {
	s := []float64{}
	return calcEvaluator{stack: &s}
}

func calcEvaluatorClear(c calcEvaluator) {
	*(c.stack) = (*(c.stack))[:0]
}

type calcValue []float64

func (c calcValue) stringifyValue() string {
	output := ""
	for _, fl := range c {
		output = fmt.Sprintf("%s %f", output, fl)
	}
	return string(output)
}

func isDigits(s string) bool {
	for _, c := range s {
		if c < '0' || c > '9' {
			return false
		}
	}
	return true
}

func (c calcEvaluator) Eval(ast Ast) Value {
	// Just call calcEval, using type conversions.
	calcAst, ok := ast.(calcAst)
	if !ok {
		return calcValue(*(c.stack))
	}
	return Value(calcEval(c, calcAst))
}

func calcEval(c calcEvaluator, ast calcAst) calcValue {
	switch ast.Data() {






	// TODO
	default:
		if isDigits(ast.Data()) {
			v, err := strconv.Atoi(ast.Data())
			if err == nil {
				// TODO
			}
		}
	}
	return calcValue(*(c.stack))
}

var calcInterpreter = Interpreter{
	Parser: calcParser{},
	Ast:    calcAst(""),
	Evaler: calcEvaluatorNew(),
}

// Assignment 12 section. You'll implement a simple game of "hot potato". A
// bunch of players surround a small area, or "court". The ball is tossed into
// the court and randomly bounces to a player who then has to hold it for a
// prescribed amount of time, after which they can toss it back in the court,
// where it will randomly bounce again. When time is up, the player holding the
// ball loses.

// This will be simulated by using goroutines for the players, a channel for the
// court, and a channel for communicating when time is up. Each player prints a
// message saying they're "out" as soon as they get the timer message. The last
// one to print is the loser.

type unit bool
const u unit = true

var courtChan chan unit
var timerChan chan unit

// The compulsory wait after receiving the ball from courtChan.
func waitABit () {
	time.Sleep(10 * time.Millisecond)
}

func player(n int) {
	// use the following line to print after getting the timer message:
	// fmt.Printf("Player %d is out.\n", n)
	for {
		// TODO
	}
}

// Play the game. The code is complete, but you need to read
// and understand it.
func play(n int) {
	courtChan = make(chan unit,1)
	courtChan <- u  // The ball is in play.
	timerChan = make(chan unit,n)
	wg := sync.WaitGroup{}
	for i := 0; i < n; i++ {
		wg.Add(1)
		go func() {
			player(i)
			wg.Done()
		}()
	}
	time.Sleep(500 * time.Millisecond)
	// Times up, notify all players.
	for i := 0; i < n; i++ {
		timerChan <- u
	}
	wg.Wait()
	println("Game over")
	// see the output to determine the loser.
}

func main() {
	// Repl(calcInterpreter)
	// play(10)
}
