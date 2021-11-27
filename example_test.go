// Copyright 2021 Jonathan Amsterdam.

package parco_test

import (
	"fmt"
	"strings"

	"github.com/jba/parco"
)

func Example() {
	p := parco.And(
		parco.Lit("the"),
		parco.Or(
			parco.Do(
				parco.Repeat(parco.Lit("big")),
				func(s *parco.State, v parco.Value) parco.Value {
					return fmt.Sprintf("big^%d", len(v.([]parco.Value)))
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
