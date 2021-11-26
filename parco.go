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


   Commit

   The Or combinator works by trying its first argument, and if that fails backtracking in the token
   stream and trying its next argument.
   TODO: finish.

*/
package parco

import (
	"fmt"
	"strconv"
)

// A Parser is a function that takes a State.
type Parser func(*State)

// State holds the state of the current parse.
type State struct {
	toks       []string
	start, pos int
	committed  bool
	// Value is passed to the Parse function and is available to user actions
	// passed to Do.
	Value interface{}
}

// AtEOF reports whether the parse has exhausted all the tokens.
func (s *State) AtEOF() bool {
	return s.pos >= len(s.toks)
}

func (s *State) current() string {
	if s.AtEOF() {
		return "end of input"
	}
	return strconv.Quote(s.toks[s.pos])
}

// Tokens returns the list of tokens parsed by the Parser given as the first argument to Do.
// It is designed to be called by functions passed as the second argument to Do.
func (s *State) Tokens() []string {
	return s.toks[s.start:s.pos]
}

// Token returns the single token parsed by the  Parser given as the first argument to Do.
// If no tokens were parsed, it returns the empty string.
// If more than one token was parsed, it panics.
// It is designed to be called by functions passed as the second argument to Do.
func (s *State) Token() string {
	if s.pos-s.start > 1 {
		panic("more than one token")
	}
	if s.pos == s.start {
		return ""
	}
	return s.toks[s.start]
}

type failure struct {
	err error
}

// Fail terminates the parse immediately with the given error.
func (s *State) Fail(err error) {
	panic(failure{err})
}

// Failf formats its arguments with fmt.Errorf, then calls Fail.
func (s *State) Failf(format string, args ...interface{}) {
	s.Fail(fmt.Errorf(format, args...))
}

// Parse uses the given Parser to parse the tokens. The value argument is put
// the Value field of the State that is passed to user-defined functions.
func Parse(p Parser, tokens []string, value interface{}) error {
	s := &State{toks: tokens, pos: 0, Value: value}
	if err := parse(p, s); err != nil {
		return err
	}
	if s.pos != len(s.toks) {
		return fmt.Errorf("unconsumed input starting at %s", s.current())
	}
	return nil
}

func parse(p Parser, s *State) (err error) {
	defer func() {
		if x := recover(); x != nil {
			if f, ok := x.(failure); ok {
				err = f.err
			} else {
				panic(x)
			}
		}
	}()
	p(s)
	return nil
}

// Literal returns a parser that parses only its argument.
func Literal(lit string) Parser {
	return func(s *State) {
		if s.AtEOF() || s.toks[s.pos] != lit {
			s.Failf("expected %q, got %s", lit, s.current())
		}
		s.pos++
	}
}

// Is returns a parser that parses a single token for which pred returns true.
// The name is used only for error messages.
func Is(name string, pred func(s string) bool) Parser {
	return func(s *State) {
		if s.AtEOF() || !pred(s.toks[s.pos]) {
			s.Failf("expected %s, got %s", name, s.current())
		}
		s.pos++
	}
}

// And invokes its argument parsers on successive parts of the input.
// It fails as soon as one of the parses fails.
func And(parsers ...Parser) Parser {
	return func(s *State) {
		for _, p := range parsers {
			if err := parse(p, s); err != nil {
				s.Fail(err)
			}
		}
	}
}

// Or tries each of its argument parsers in turn on the same input, succeeding
// as soon as the first succeeds and failing if they all fail.
// The Commit parser modifies that behavior; if an argument parser calls Commit,
// then Or fails as soon as that parser fails instead of trying the next argument.
func Or(parsers ...Parser) Parser {
	return func(s *State) {
		start := s.pos
		defer func(c bool) { s.committed = c }(s.committed)
		s.committed = false

		for _, p := range parsers {
			err := parse(p, s)
			if err == nil {
				return
			}
			if s.committed {
				s.Fail(err)
			}
			s.pos = start
		}
		s.Failf("parse failed at %q", s.current())
	}
}

var (
	// Empty parses the empty input.
	Empty Parser = func(*State) {}

	// Commit causes Or to stop trying alternatives on an error.
	// See Or's documentation for more.
	Commit Parser = func(s *State) { s.committed = true }

	// Any parses any single token.
	Any Parser = func(s *State) {
		if s.AtEOF() {
			s.Failf("unexpected end of unput")
		}
		s.pos++
	}
)

// Optional parses either what p parses, or nothing.
// It is equivalent to Or(p, Empty).
func Optional(p Parser) Parser {
	return Or(p, Empty)
}

// Repeat calls p repeatedly until it fails.
func Repeat(p Parser) Parser {
	// We can't write
	//  Or(And(p, Repeat(p)), Empty)
	// as we would like. Go is applicative-order, so the recursive call to Repeat happens
	// immediately and we have infinite recursion. We must delay the recursion.
	return Or(
		And(p, func(s *State) { Repeat(p)(s) }),
		Empty)
}

// List parses a non-empty list of items separate by seps.
func List(item, sep Parser) Parser {
	return And(item, Repeat(And(sep, item)))
}

// Do first parses some tokens using p. If p succeeds, then it calls f with the
// parse state. Calling Tokens or Token on that state will return the tokens
// parsed by p. The function can also access the Value field of State to read
// and write the value passed as the third argument of Parse. It can call the
// Fail and Failf methods of State to signal an error.
func Do(p Parser, f func(*State)) Parser {
	return func(s *State) {
		start := s.pos
		p(s)
		s.start = start
		f(s)
	}
}
