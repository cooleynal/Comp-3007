package main

type E interface {
	eval(Env) V
}

type IntE int

type BoolE bool

type AtomE string

type ListE []E

func (x IntE)eval(Env) V {
	return (IntV(int(x)))
}

func (x BoolE)eval(Env) V {
	return (BoolV(bool(x)))
}

func (x AtomE)eval(env Env) V {
	return (env[string(x)])
}

type V interface {
	isV()
}

type NilV struct{}

type IntV int

type BoolV bool

type ConsV struct { car V; cdr V }

type ClosureV struct { env Env; params []string; body E }

func (NilV)isV() {}

func (IntV)isV(){}

func (BoolV)isV(){}

func (ConsV)isV(){}

func (ClosureV)isV(){}

type Env map[string]V

var emptyEnv = Env{}

func (x ListE)eval(env Env) V {
	switch x[0] {

		case AtomE("list"):
		return mkListV(env, x[1:])

		case AtomE("if"):
		if x[1].eval(env) == IntV(0) {
			return x[2].eval(env)
		} else { return x[3].eval(env) }

		case AtomE("cons"):
		return ConsV{car: x[1].eval(env), cdr: x[2].eval(env)}

		case AtomE("car"):
		return x[1].eval(env).(ConsV).car

		case AtomE("cdr"):
		return x[1].eval(env).(ConsV).cdr

		case AtomE("eq?"):
		m, okm := x[1].eval(env).(IntV)
		n, okn := x[2].eval(env).(IntV)
		return BoolV (okm && okn && m == n)

		case AtomE("null?"):
		_, ok := x[1].eval(env).(NilV)
		return BoolV(ok)

		case AtomE("+"):
		return IntV(x[1].eval(env).(IntV) + x[2].eval(env).(IntV))

		case AtomE("-"):
		return IntV(x[1].eval(env).(IntV) - x[2].eval(env).(IntV))

		case AtomE("*"):
		return IntV(x[1].eval(env).(IntV) * x[2].eval(env).(IntV))

		default: panic("unknown expression")
	}
}

func mkListV(env Env, l []E) V {
	if len(l) == 0 {
		return NilV{}
	} else {
		return ConsV{car: l[0].eval(env), cdr: mkListV(env, l[1:])}
	}
}

func l(es ...E) E {
	return ListE(es)
}

var one, two, three = IntE(1), IntE(2), IntE(3)
var plus, minus, times = AtomE("+"), AtomE("-"), AtomE("*")

var eg =l(times, l(plus, two,three), three)

// func main(){
// 	fmt.Println(eg.eval(emptyEnv))
// }
