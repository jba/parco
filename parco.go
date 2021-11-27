// Copyright 2021 Jonathan Amsterdam.

/* Package parco is a parser combinator library for Go.
   It is suitable for parsing short texts, like embedded languages.


   Basics

   First, construct the parser by using the combinator functions to
   describe the grammar. For example, the simple grammar

	   the (big* | small) dog

   allows phrases like "the dog", "the big dog", "the big big dog", and "the small dog".
   The parser for that grammar looks like

	 p := And(
	   Literal("the"),
	   Or(
		 Repeat(Literal("big")),
		 Literal("small")),
	   Literal("dog"))

   To parse a string, call Parse:

     err := Parse(p, strings.Fields("the big dog"), nil)

   Parse takes as its input a slice of strings. If you just want to split a
   string on whitespace, you can use strings.Fields as is done here. For more
   sophisticated uses you may want a lexer like github.com/jba/lexer.


   Actions

   To associate actions with a parse, use the Do function to associate a
   function with a parser. The function takes a State, which provides the
   sequence of tokens that the parser traversed and also gives access to the
   value passed as the third argument to Parse.

   For example, to count the number of "big"s in an input string, we could
   modify the above parser like so:

	 p := And(
	   Literal("the"),
	   Or(
		 Do(Repeat(Literal("big")), func(s *State) { *(s.Value.(*int)) = len(s.Tokens()) }),
		 Literal("small")),
	   Literal("dog"))

   and invoke Parse like so:

     var n int
     err := Parse(p, ..., &n)
     if err != nil {
       return err
     }
     fmt.Printf("there were %d 'big's in the input\n", n)


   Cut

   The Or combinator works by trying its first argument, and if that fails backtracking in the token
   stream and trying its next argument.
   TODO: finish.

*/
package parco

import (
	"errors"
	"fmt"
	"strconv"
)

type Value = interface{}

// A Parser is a function that takes a State and returns some value.
type Parser func(*state) (Value, error)

// state holds the state of the current parse.
type state struct {
	toks       []string
	start, pos int
	committed  bool
}

// atEOF reports whether the parse has exhausted all the tokens.
func (s *state) atEOF() bool {
	return s.pos >= len(s.toks)
}

func (s *state) current() string {
	if s.atEOF() {
		return "end of input"
	}
	return strconv.Quote(s.toks[s.pos])
}

// // Values returns the list of values returned by parsers.
// // It is designed to be called by functions passed as the second argument to Do.
// func (s *State) Values() []interface{} {
// 	return s.vals
// }

// Token returns the single token parsed by the  Parser given as the first argument to Do.
// If no tokens were parsed, it returns the empty string.
// If more than one token was parsed, it panics.
// It is designed to be called by functions passed as the second argument to Do.
// func (s *State) Token() string {
// 	if s.pos-s.start > 1 {
// 		panic("more than one token")
// 	}
// 	if s.pos == s.start {
// 		return ""
// 	}
// 	return s.toks[s.start]
// }

type failure struct {
	err error
}

// Fail terminates the parse immediately with the given error.
// func (s *State) Fail(err error) {
// 	panic(failure{err})
// }

// // Failf formats its arguments with fmt.Errorf, then calls Fail.
// func (s *State) Failf(format string, args ...interface{}) {
// 	s.Fail(fmt.Errorf(format, args...))
// }

// Parse uses the given Parser to parse the tokens. The value argument is put
// the Value field of the State that is passed to user-defined functions.
func Parse(p Parser, tokens []string) (Value, error) {
	s := &state{toks: tokens, pos: 0}
	val, err := p(s)
	if err != nil {
		return nil, err
	}
	if s.pos != len(s.toks) {
		return nil, fmt.Errorf("unconsumed input starting at %s", s.current())
	}
	return val, nil
}

// Lit returns a parser that parses only its argument.
func Lit(lit string) Parser {
	return func(s *state) (Value, error) {
		if s.atEOF() || s.toks[s.pos] != lit {
			return nil, fmt.Errorf("expected %q, got %s", lit, s.current())
		}
		s.pos++
		return s.toks[s.pos-1], nil
	}
}

// Is returns a parser that parses a single token for which pred returns true.
// The name is used only for error messages.
func Is(name string, pred func(s string) bool) Parser {
	return func(s *state) (Value, error) {
		if s.atEOF() || !pred(s.toks[s.pos]) {
			return nil, fmt.Errorf("expected %s, got %s", name, s.current())
		}
		s.pos++
		return s.toks[s.pos-1], nil
	}
}

// And returns a parser that invokes its argument parsers in succession,
// and fails as soon as one of the parsers fails.
// The parser returns a slice of the argument parsers' non-nil values.
func And(parsers ...Parser) Parser {
	return func(s *state) (Value, error) {
		var vals []Value
		for _, p := range parsers {
			val, err := p(s)
			if err != nil {
				return nil, err
			}
			if val != nil {
				vals = append(vals, val)
			}
		}
		return vals, nil
	}
}

// Or tries each of its argument parsers in turn on the same input, succeeding
// as soon as the first succeeds and failing if they all fail.
// The Commit parser modifies that behavior; if an argument parser calls Commit,
// then Or fails as soon as that parser fails instead of trying the next argument.
func Or(parsers ...Parser) Parser {
	return func(s *state) (Value, error) {
		start := s.pos
		defer func(c bool) { s.committed = c }(s.committed)
		s.committed = false

		for _, p := range parsers {
			val, err := p(s)
			if err != nil && s.committed {
				return nil, err
			}
			if err == nil {
				return val, nil
			}
			s.pos = start
		}
		return nil, fmt.Errorf("parse failed at %q", s.current())
	}
}

var (
	// Empty parses the empty input and returns nil.
	Empty Parser = func(*state) (Value, error) { return nil, nil }

	// Cut causes Or to stop trying alternatives on an error.
	// See Or's documentation for more.
	Cut Parser = func(s *state) (Value, error) { s.committed = true; return nil, nil }

	// Any parses any single token.
	Any Parser = func(s *state) (Value, error) {
		if s.atEOF() {
			return nil, errors.New("unexpected end of unput")
		}
		s.pos++
		return s.toks[s.pos-1], nil
	}
)

// Opt parses either what p parses, or nothing.
// It is equivalent to Or(p, Empty).
func Opt(p Parser) Parser {
	return Or(p, Empty)
}

// Repeat calls p repeatedly until it fails.
func Repeat(p Parser) Parser {
	// We can't write
	//  Or(And(p, Repeat(p)), Empty)
	// as we would like. Go is applicative-order, so the recursive call to Repeat happens
	// immediately and we have infinite recursion. We must delay the recursion, which
	// we could do with `func(s *State) Value { return Repeat(p)(s) }`.
	// Another problem is that the return value would be a nested slice instead of a flat
	// one.
	// So it is cleaner to write this as a loop.
	return func(s *state) (Value, error) {
		var vals []Value
		or := Or(p, Empty)
		for {
			val, err := or(s)
			if err != nil {
				return nil, err
			}
			if val == nil {
				return vals, nil
			}
			vals = append(vals, val)
		}
	}
}

// List returns a parser that parses a non-empty list of items separate by sep.
// The parser returns a slice of the items' values, ignoring the seps' values.
func List(item, sep Parser) Parser {
	return Do(
		And(item, Repeat(Do(
			And(sep, item),
			func(v Value) (Value, error) { return v.([]Value)[1], nil }))),
		func(v Value) (Value, error) {
			// v is a pair of [item, slice of items].
			// Flatten it.
			vals := v.([]Value)
			return append([]Value{vals[0]}, vals[1].([]Value)...), nil
		})
}

// Do first parses some tokens using p. If p succeeds, then it calls f with the
// parse state and the value of p. The function's return value is the value of Do.
func Do(p Parser, f func(Value) (Value, error)) Parser {
	return func(s *state) (Value, error) {
		val, err := p(s)
		if err != nil {
			return nil, err
		}
		return f(val)
	}
}
