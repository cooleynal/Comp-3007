package main

type V interface{
	isV() bool
}

type VBool struct{ b bool }
type VInt struct{ k int }
type VCons struct{ car V; cdr V }
type VNil struct{}

func (v VBool) isV() bool { return true }
func (v VInt) isV() bool { return true }
func (v VCons) isV() bool { return true }
func (v VNil) isV() bool { return true }

type E interface{
	eval() V
}

type Atom struct{ s string }
type List struct{ es []E }
type Number struct{ k int }
type String struct{ s string }
type Nil struct{}
type Bool struct{ b bool }

func main() {}
