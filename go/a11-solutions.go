// Don't change the package name below nor the name of this file.
package main

// Feel free to add imports but only from the standard library. You
// shouldn't need anything more than the ones below.
import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
)


//////////////////////////////////////////////////////////////////////
// The assignment is to create an interpreter framework using
// interfaces and then instantiate it for a simple Scheme interpreter.
//
// Interpreters here have four main parts: a lexer, a parser, an ast,
// and an evaluator. The lexer breaks the input into tokens, the parser
// constructs an abstract syntax tree (ast) from the tokens, and the
// evaluator interprets the ast to produce a result.
//
// The Scheme interpreter is a shabby near-useless one. All it can do
// is add, multiply and subtract integers. But as Microsoft says,
// "good enough."
//
// Fill in the missing parts, indicated by "TODO". The main program
// is a read-eval-print loop you can use for testing.
//
// The assignment has been designed to not require detailed knowledge of
// Go. In particular, pointers have been avoided.
// What you'll need to know to complete this assignment:
// - the basics of structs and interfaces
// - the basics of slices: slice literals, append, slicing
//   slices, len
// - multiple return values for functions
// - "for" loops for slices, and "for {}" for loops
//	 that end when "break" is called
// - the use of type names as conversion functions
// - typecasting with "x.(T)"
// You can learn about these by asking ChatGPT. The standard library
// documentation is also a good resource.
//
// Some advice: a lot of the code is already written for you. Read it
// carefully and make sure you understand it. It is very similar to
// code you will need to write.

//////////////////////////////////////////////////////////////////////
// An interpreter framework using interfaces /////////////////////////
//////////////////////////////////////////////////////////////////////

// The LexNext method returns the next token ("" if none) and the
// remaining portion of the given byte array slice.
type Lexer interface {
	LexNext([]byte) (string, []byte)
}

// An interface for abstract syntax trees.
// Note: the interface doesn't having anything for onstructing an AST.
// This is normal in Go since iterfaces generally collect together different
// kinds of data types, each with their own way of constructing elements.
// The interface *methods* are the shared behaviour of the different types.
type Ast interface {
	Data() string // all data needed for interpreting the root node
	Parts() []Ast // the subtrees of the root node
}

type Parser interface {
	Parse([]string) Ast
}

type Value interface {
	stringifyValue() string
}

type Evaluator interface {
	Eval(Ast) Value
}

type Interpreter struct {
	Lexer  Lexer
	Ast    Ast
	Parser Parser
	Evaler Evaluator
}

// Repeatedly calls LexNext until no tokens remain and return the
// accumulated tokens.
func Lex(lexer Lexer, str string) []string {
	tokens := []string{}
	remainingStr := []byte(str)
	var token string
	for {
		token, remainingStr = lexer.LexNext(remainingStr)
		if token == "" {
			return tokens
		}
		tokens = append(tokens, token)
	}
}

// Run the interpreter on a string input, printing out the result
func Run(i Interpreter, str string) {
	ast := i.Parser.Parse(Lex(i.Lexer, str))
	fmt.Printf("%v\n", i.Evaler.Eval(ast))
}

// Run a read-eval-print loop that reads input from the user, runs
// Run on it, and prints the result.
func Repl(i Interpreter) {
	for {
		reader := bufio.NewReader(os.Stdin)
		fmt.Print("\"repl\" >  ")
		str, err := reader.ReadString('\n')
		if err != nil {
			panicWith("Repl", err)
		}
		Run(i, str)
	}
}

func main() {
	Repl(schemeInterpreter)
}

// Print an Ast in a readable form. For debugging.
func printAst(t Ast) {
	if len(t.Parts()) == 0 {
		print(t.Data())
	} else {
		print("Node[" + t.Data() + "]" + "(")
		for i, p := range t.Parts() {
			printAst(p)
			if i < len(t.Parts())-1 {
				print(", ")
			}
		}
		print(")")
	}
}

////////////////////////////////////////////////////////////////////////
// A simple Scheme interpreter as an instance of the above interfaces //
////////////////////////////////////////////////////////////////////////

// There's no state to keep track of so we use a minimal struct for our
// instance of Lexer. Similarly for the Parser and Evaluator below.
type schemeLexer struct{}

type schemeParser struct{}

type schemeAst struct {
	parts []schemeAst
	data  string
}

type schemeVal int

type schemeEvaluator struct{}

var schemeInterpreter = Interpreter{
	Lexer:  schemeLexer{},
	Parser: schemeParser{},
	Ast:    schemeAst{},
	Evaler: schemeEvaluator{},
}

func (sl schemeLexer) LexNext(chars []byte) (string, []byte) {
	if len(chars) == 0 {
		return "", nil
	}
	_, chars = takeWhile(chars, isWhitespace)
	c := chars[0]
	var token []byte
	var rest []byte
	switch true {
	case c == '(' || c == ')':
		token, rest = chars[0:1], chars[1:]
	case isNumeric(c):
		token, rest = takeWhile(chars, isNumeric)
	default:
		token, rest = takeWhile(chars, func(c byte) bool { return !isNumeric(c) && !isWhitespace(c) })
	}
	return string(token), rest
}

func (sa schemeAst) Data() string {
	return sa.data
}

func (sa schemeAst) Parts() []Ast {
	result := make([]Ast, len(sa.parts))
	for i, _ := range sa.parts {
		result[i] = sa.parts[i]
	}
	return result
}

func (sp schemeParser) Parse(tokens []string) Ast {
	ast, rest := schemeParse(tokens)
	if len(rest) != 0 {
		panicWith("schemeParser", "")
	}
	return ast
}

func schemeParse(tokens []string) (schemeAst, []string) {
	if len(tokens) == 0 || tokens[0] == ")" {
		panic(tokens)
	}
	if tokens[0] == "(" {
		group := getParend(tokens)
		groupSize := len(group)
		asts := []schemeAst{}
		for {
			if len(group) == 0 {
				break
			} else {
				ast, rest := schemeParse(group)
				asts = append(asts, ast)
				group = rest
			}
		}
		return schemeAst{parts: asts, data: ""}, tokens[groupSize+2:]
	}
	return schemeAst{parts: []schemeAst{}, data: tokens[0]}, tokens[1:]
}

func (sv schemeVal) stringifyValue() string {
	return strconv.Itoa(int(sv))
}

func (se schemeEvaluator) Eval(t Ast) Value {
	schemeT, ok := t.(schemeAst)
	if !ok {
		panicWith("Eval: expected schemeAst t: ", t)
	}
	return schemeVal(schemeEval(schemeT))
}

func schemeEval(t schemeAst) int {
	if len(t.parts) == 0 {
		return stringToInt(t.Data())
	}
	opAst := t.parts[0]
	if len(opAst.parts) != 0 {
		// first position must be operator/identifier
		panicWith("schemeEval:0", opAst)
	}
	op := opAst.Data()
	args := []int{}
	for _, arg := range t.parts[1:] {
		args = append(args, schemeEval(arg))
	}
	var result int
	switch op {
	case "+":
		result = args[0] + args[1]
	case "*":
		result = args[0] * args[1]
	case "-":
		result = args[0] - args[1]
	default:
		panic(op)
	}
	return result
}

////////////////////////////////////////////////////////////////////////////////////////
// Some generic utilities //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////

func isWhitespace(c byte) bool {
	return slices.Contains([]byte{' ', '\t', '\n', '\r'}, c)
}

func isNumeric(c byte) bool {
	return slices.Contains([]byte{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}, c)
}

// Removes and returns the longest prefix of s all of whose characters
// satisfy the predicate.
func takeWhile[T any](s []T, f func(c T) bool) ([]T, []T) {
	i := slices.IndexFunc(s, func(c T) bool { return !f(c) })
	if i < 0 {
		i = 0
	}
	return s[0:i], s[i:]
}

func panicWith(s string, x any) {
	println(s)
	panic(x)
}

func getParend(tokens []string) []string {
	if len(tokens) == 0 || tokens[0] != "(" {
		panicWith("getParend:1", tokens)
	}
	count := 0
	matchingRight := 0
	for i, c := range tokens {
		switch true {
		case c == ")":
			count = count - 1
		case c == "(":
			count = count + 1
		default:
		}
		if count == 0 {
			matchingRight = i
			break
		}
	}
	if matchingRight == 0 {
		panicWith("getParend:2", tokens)
	}
	return tokens[1:matchingRight]
}

func stringToInt(s string) int {
	i, _ := strconv.Atoi(s)
	return i
}
