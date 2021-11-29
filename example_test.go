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

func Example_calculator() {
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
