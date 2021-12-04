// Copyright 2021 Jonathan Amsterdam.

package parco_test

import (
	"fmt"

	"github.com/jba/parco"
)

func Example() {
	p := parco.And(
		parco.Word("the"),
		parco.Or(
			parco.Word("small"), // must be first, because Repeat will always succeed
			parco.Do(
				parco.Repeat(parco.Word("big")),
				func(v []string) string { return fmt.Sprintf("big^%d", len(v)) }),
		),
		parco.Word("dog"))

	for _, phrase := range []string{
		"the dog",
		"the small dog",
		"the big dog",
		"the big big big dog",
	} {
		val, err := p.Parse(phrase)
		if err != nil {
			panic(err)
		}
		fmt.Println(val)
	}
	// Output:
	// [the big^0 dog]
	// [the small dog]
	// [the big^1 dog]
	// [the big^3 dog]
}

// A four-function calculator where the calculations happen during parsing.
func Example_immediate_calculator() {
	type pair struct {
		op  string
		num float64
	}

	var (
		expr, factor parco.Parser[pair]
		Eq           = parco.Equal
		Repeat       = parco.Repeat[pair]
	)

	factor = parco.Or(
		parco.Do(parco.Float, func(f float64) pair { return pair{"", f} }),
		parco.And2(Eq("-"), parco.Ptr(&factor),
			func(_ string, p pair) pair { return pair{"", -p.num} }),
		parco.And3(Eq("("), parco.Ptr(&expr), Eq(")"),
			func(_ string, p pair, _ string) pair { return pair{"", p.num} }),
	)
	// term ::= factor | term (* | /) factor
	// but we can't write it that way because the Or would succeed and
	// return after the first factor.
	term := parco.And2(factor,
		Repeat(parco.And2(parco.Or(Eq("*"), Eq("/")), factor,
			func(op string, p pair) pair {
				return pair{op, p.num}
			})),
		func(p pair, ps []pair) pair {
			f := p.num
			for _, p := range ps {
				switch p.op {
				case "*":
					f *= p.num
				case "/":
					f /= p.num
				}
			}
			return pair{"", f}
		})

	expr = parco.And2(term,
		Repeat(parco.And2(parco.Or(Eq("+"), Eq("-")), term,
			func(op string, p pair) pair {
				return pair{op, p.num}
			})),
		func(p pair, ps []pair) pair {
			f := p.num
			for _, p := range ps {
				switch p.op {
				case "+":
					f += p.num
				case "-":
					f -= p.num
				}
			}
			return pair{"", f}
		})

	for _, in := range []string{
		"2", "- 3", "2 * 3", "2 * - 3", "2 * -3", "2 * 3 / 4", "1 + 2 * 3", "( 1 + 2 ) * 3", "((3) )",
	} {
		val, err := expr.Parse(in)
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s = %g\n", in, val.num)
	}

	// Output:
	// 2 = 2
	// - 3 = -3
	// 2 * 3 = 6
	// 2 * - 3 = -6
	// 2 * -3 = -6
	// 2 * 3 / 4 = 1.5
	// 1 + 2 * 3 = 7
	// ( 1 + 2 ) * 3 = 9
	// ((3) ) = 3
}

// A four-function calculator where the parser builds up a tree for
// later evaluation.
func Example_delayed_calculator() {
	type node struct {
		op          string
		num         int64 // when op == ""
		left, right *node
	}

	var (
		expr, factor parco.Parser[*node]
		Eq           = parco.Equal
		Repeat       = parco.Repeat[*node]
	)

	factor = parco.Or(
		parco.Do(parco.Int, func(i int64) *node { return &node{op: "", num: i} }),
		parco.And2(Eq("-"), parco.Ptr(&factor),
			func(_ string, n *node) *node { n.num = -n.num; return n }),
		parco.And3(Eq("("), parco.Ptr(&expr), Eq(")"),
			func(_ string, n *node, _ string) *node { return n }),
	)

	p := func(op1, op2 string, argp parco.Parser[*node]) parco.Parser[*node] {
		return parco.And2(argp,
			Repeat(parco.And2(parco.Or(Eq(op1), Eq(op2)), argp,
				func(op string, n *node) *node {
					return &node{op: op, right: n}
				})),
			func(left *node, ns []*node) *node {
				if len(ns) == 0 {
					return left
				}
				for _, n := range ns {
					n.left = left
					left = n
				}
				return ns[len(ns)-1]
			})
	}

	term := p("*", "/", factor)
	expr = p("+", "-", term)

	var eval func(n *node) int64
	eval = func(n *node) int64 {
		if n.op == "" {
			return n.num
		}
		l := eval(n.left)
		r := eval(n.right)
		switch n.op {
		case "+":
			return l + r
		case "-":
			return l - r
		case "*":
			return l * r
		case "/":
			return l / r
		default:
			panic("bad op")
		}
	}

	for _, in := range []string{
		"2", "- 3", "2 * 3", "2 * - 3", "2 * -3", "2 * 3 / 4", "1 + 2 * 3", "( 1 + 2 ) * 3", "((3) )",
	} {
		n, err := expr.Parse(in)
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s = %d\n", in, eval(n))
	}

	// Output:
	// 2 = 2
	// - 3 = -3
	// 2 * 3 = 6
	// 2 * - 3 = -6
	// 2 * -3 = -6
	// 2 * 3 / 4 = 1
	// 1 + 2 * 3 = 7
	// ( 1 + 2 ) * 3 = 9
	// ((3) ) = 3

}
