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
			parco.Repeat(parco.Word("big")).Do(func(v []parco.Value) parco.Value {
				return fmt.Sprintf("big^%d", len(v))
			}),
			parco.Word("small")),
		parco.Word("dog"))
	val, err := p.Parse("the big big big dog")
	if err != nil {
		panic(err)
	}
	fmt.Println(val)

	// Output: [the big^3 dog]
}

func Example_calculator() {
	eval := func(vs []parco.Value) (parco.Value, error) {
		// Only one element: just the factor.
		if len(vs) == 1 {
			return vs[0], nil
		}
		// A slice of [op, arg] pairs.
		f := vs[0].(float64)
		for _, e := range vs[1].([]parco.Value) {
			opArg := e.([]parco.Value)
			arg := opArg[1].(float64)
			switch opArg[0].(string) {
			case "+":
				f += arg
			case "-":
				f -= arg
			case "*":
				f *= arg
			case "/":
				f /= arg
			default:
				return nil, fmt.Errorf("bad op: %q", opArg[0])
			}
		}
		return f, nil
	}

	var (
		expr, factor parco.Parser
		eq           = parco.Equal
		or           = parco.Or
		and          = parco.And
		repeat       = parco.Repeat
	)
	type value = parco.Value

	factor = or(
		parco.Float,
		and(eq("-"), parco.Ptr(&factor)).Do(func(vs []value) value {
			return -vs[1].(float64)
		}),
		and(eq("("), parco.Ptr(&expr), eq(")")).Do(func(vs []value) value {
			return vs[1]
		}))

	term := and(factor, repeat(and(or(eq("*"), eq("/")), factor))).Do(eval)
	expr = and(term, repeat(and(or(eq("+"), eq("-")), term))).Do(eval)

	for _, in := range []string{
		"2", "- 3", "2 * 3", "2 * - 3", "2 * 3 / 4", "1 + 2 * 3", "( 1 + 2 ) * 3", "( ( 3 ) )",
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
	// 2 * 3 / 4 = 1.5
	// 1 + 2 * 3 = 7
	// ( 1 + 2 ) * 3 = 9
	// ( ( 3 ) ) = 3
}
