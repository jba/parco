// Copyright 2021 Jonathan Amsterdam.

package parco_test

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/jba/parco"
)

func Example() {
	p := parco.And(
		parco.Lit("the"),
		parco.Or(
			parco.Repeat(parco.Lit("big")).Do(func(v parco.Value) (parco.Value, error) {
				return fmt.Sprintf("big^%d", len(v.([]parco.Value))), nil
			}),
			parco.Lit("small")),
		parco.Lit("dog"))
	val, err := p.Parse(strings.Fields("the big big big dog"))
	if err != nil {
		panic(err)
	}
	fmt.Println(val)

	// Output: [the big^3 dog]
}

func Example_calculator() {
	eval := func(v parco.Value) (parco.Value, error) {
		vs := v.([]parco.Value)
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
		lit          = parco.Lit
		or           = parco.Or
		and          = parco.And
		repeat       = parco.Repeat
	)
	type value = parco.Value

	factor = or(
		parco.Any.Do(func(v value) (value, error) {
			return strconv.ParseFloat(v.(string), 64)
		}),
		and(lit("-"), parco.Ptr(&factor)).Do(func(v value) (value, error) {
			return -v.([]value)[1].(float64), nil
		}),
		and(lit("("), parco.Ptr(&expr), lit(")")).Do(func(v value) (value, error) {
			return v.([]value)[1], nil
		}))

	term := and(factor, repeat(and(or(lit("*"), lit("/")), factor))).Do(eval)
	expr = and(term, repeat(and(or(lit("+"), lit("-")), term))).Do(eval)

	for _, in := range []string{
		"2", "- 3", "2 * 3", "2 * - 3", "2 * 3 / 4", "1 + 2 * 3", "( 1 + 2 ) * 3", "( ( 3 ) )",
	} {
		val, err := expr.Parse(strings.Fields(in))
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
