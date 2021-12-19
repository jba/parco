// Copyright 2021 Jonathan Amsterdam.

package parco_test

import (
	"fmt"

	"github.com/jba/parco"
)

// A parser for the grammar "the (small | big*) dog".
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
	var (
		expr, factor parco.Parser[float64]
		Eq           = parco.Equal
	)

	factor = parco.Or(
		parco.Float,
		parco.And2(Eq("-"), parco.Ptr(&factor),
			func(_ string, f float64) float64 { return -f }),
		parco.And3(Eq("("), parco.Ptr(&expr), Eq(")"),
			func(_ string, f float64, _ string) float64 { return f }),
	)

	eval := func(x float64, op string, y float64) float64 {
		switch op {
		case "+":
			return x + y
		case "-":
			return x - y
		case "*":
			return x * y
		case "/":
			return x / y
		default:
			panic("bad op")
		}
	}

	// term ::= factor | term (* | /) factor
	// but we can't write it that way because the Or would succeed and
	// return after the first factor.

	type pair struct {
		op  string
		arg float64
	}

	seq := func(op1, op2 string, argp parco.Parser[float64]) parco.Parser[float64] {
		return parco.LeftFold(
			argp,
			parco.And2(parco.Or(Eq(op1), Eq(op2)), argp, func(op string, arg float64) pair {
				return pair{op, arg}
			}),
			func(left float64, p pair) float64 {
				return eval(left, p.op, p.arg)
			})
	}

	term := seq("*", "/", factor)
	expr = seq("+", "-", term)

	for _, in := range []string{
		"2", "- 3", "2 * 3", "2 * - 3", "2 * -3", "2 * 3 / 4", "1 + 2 * 3", "( 1 + 2 ) * 3", "((3) )",
	} {
		val, err := expr.Parse(in)
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s = %g\n", in, val)
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
	)

	factor = parco.Or(
		parco.Do(parco.Int, func(i int64) *node { return &node{op: "", num: i} }),
		parco.And2(Eq("-"), parco.Ptr(&factor),
			func(_ string, n *node) *node { n.num = -n.num; return n }),
		parco.And3(Eq("("), parco.Ptr(&expr), Eq(")"),
			func(_ string, n *node, _ string) *node { return n }),
	)

	p := func(op1, op2 string, argp parco.Parser[*node]) parco.Parser[*node] {
		return parco.LeftFold(
			argp,
			parco.And2(parco.Or(Eq(op1), Eq(op2)), argp,
				func(op string, right *node) *node {
					return &node{op: op, right: right}
				}),
			func(n1, n2 *node) *node {
				n2.left = n1
				return n2
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
