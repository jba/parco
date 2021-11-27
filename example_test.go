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
			parco.Do(
				parco.Repeat(parco.Lit("big")),
				func(v parco.Value) (parco.Value, error) {
					return fmt.Sprintf("big^%d", len(v.([]parco.Value))), nil
				}),
			parco.Lit("small")),
		parco.Lit("dog"))
	val, err := parco.Parse(p, strings.Fields("the big big big dog"))
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

	factor := parco.Do(parco.Any, func(v parco.Value) (parco.Value, error) {
		f, err := strconv.ParseFloat(v.(string), 64)
		return f, err
	})
	term := parco.Do(
		parco.And(
			factor,
			parco.Repeat(
				parco.And(
					parco.Or(parco.Lit("*"), parco.Lit("/")),
					factor))),
		eval)
	expr := parco.Do(
		parco.And(
			term,
			parco.Repeat(
				parco.And(
					parco.Or(parco.Lit("+"), parco.Lit("-")),
					term))),
		eval)

	for _, in := range []string{
		"2", "2 * 3", "2 * 3 / 4", "1 + 2 * 3",
	} {
		val, err := parco.Parse(expr, strings.Fields(in))
		if err != nil {
			panic(err)
		}
		fmt.Printf("%s = %g\n", in, val)
	}

	// Output:
	// 2 = 2
	// 2 * 3 = 6
	// 2 * 3 / 4 = 1.5
	// 1 + 2 * 3 = 7

}
