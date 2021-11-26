// Copyright 2021 Jonathan Amsterdam.

package parco_test

import (
	"fmt"
	"strings"

	"github.com/jba/parco"
)

func Example() {
	p := parco.And(
		parco.Literal("the"),
		parco.Or(
			parco.Do(
				parco.Repeat(parco.Literal("big")),
				func(s *parco.State) { *(s.Value.(*int)) = len(s.Tokens()) }),
			parco.Literal("small")),
		parco.Literal("dog"))
	var n int
	err := parco.Parse(p, strings.Fields("the big big big dog"), &n)
	if err != nil {
		panic(err)
	}
	fmt.Printf("there are %d 'big's in the input.\n", n)

	// Output: there are 3 'big's in the input.
}
