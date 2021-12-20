// Copyright 2021 Jonathan Amsterdam.

/*
Package parco is a parser combinator library for Go.
It is suitable for parsing short texts, like embedded languages.


Basics

First, construct the parser by using the combinator functions to
describe the grammar. For example, the simple grammar

    the (big* | small) dog

allows phrases like "the dog", "the big dog", "the big big dog", and "the small dog".
The parser for that grammar looks like

	p := And(
	  Word("the"),
	  Or(
	    Repeat(Word("big")),
	    Word("small"),
      ),
	  Word("dog"))

To parse a string, call Parse:

     val, err := p.Parse("the big dog")

The value of this parse will be a slice of the input words:

     ["the", "big", "dog"]


Actions

Every Parser returns a Value, which can be anything. (Value is an alias for
interface{}.) The value of the top-level parser is returned by Parser.Parse,
but you can modify or act upon any parser's value by associating an action
with it. To do so, call its Do method with a function. The function can
have a few different signatures; most generally, it takes a Value and returns
(Value, error). The argument is the parser's value, and the returned
value replaces it. Returning an error immediately fails the entire parse.

For example, to replace consecutive "big"s in the input string, we could modify
the above parser like so (using a simpler, alternative function signature):

	 p := And(
	   Word("the"),
	   Or(
			Repeat(Word("big")).Do(func(v []Value) Value {
				return fmt.Sprintf("big^%d", len(v))
			}),
		    Word("small")),
	   Word("dog"))

The value of p.Parse("the big big big dog") is

     ["the", "big^3", "dog"]


Cut

The Or combinator works by trying its first argument, and if that fails, then
backtracking in the input stream and trying its next argument. That can be
expensive, but the bigger problem is that errors are unhelpful. For example,
when the parser

     Or(And(Word("limit"), Int), Word("other"))

is applied to "limit x", the resulting error is

     parse failed at index 0 ("limit...")

The real problem is that the token after "limit" was not a valid integer, but that
error serves only to trigger the backtracking and isn't retained.

Taking an idea from logic languages, parco provides a "cut" operator that commits
the parse to a particular choice. By adding Cut after "limit", like so:

     Or(And(Word("limit"), Cut, Int), Word("other"))

the parser will not backtrack past the cut, and the input "limit x" produces
the error "expected integer".

You will usually want to add Cut after the first token of a particular parsing
choice, because most modern formal languages are designed to be parsed by
looking ahead only a single token. But you can put it anywhere you like.
*/
package parco

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// State holds the state of the current parse.
type State struct {
	input      string
	start, pos int
	committed  bool
	skipPred   func(rune) bool
}

func newState(input string) *State {
	return &State{
		input:    input,
		pos:      0,
		skipPred: unicode.IsSpace,
	}
}

func (s *State) skip() {
	if s.skipPred == nil {
		return
	}
	for i, r := range s.input[s.pos:] {
		if !s.skipPred(r) {
			s.pos += i
			return
		}
	}
	s.pos = len(s.input)
}

func (s *State) position() string {
	if s.pos == len(s.input) {
		return "end of input"
	}
	const n = 5
	t := s.input[s.pos:]
	if len(t) > n {
		t = t[:n] + "..."
	}
	return fmt.Sprintf("index %d (%q)", s.pos, t)
}

// A Parser is a function that takes a state, tries to consume so input, and
// returns some value.
type Parser[T any] func(*State) T

// Parse uses the given Parser to parse the tokens. The value argument is put
// the Value field of the State that is passed to user-defined functions.
// By default, whitespace in the input is skipped. Call Skipping to change
// that behavior.
func (p Parser[T]) Parse(input string) (z T, _ error) {
	s := newState(input)
	val, err := Catch(p, s)
	if err != nil {
		return z, err
	}
	s.skip()
	if s.pos != len(s.input) {
		return z, fmt.Errorf("unconsumed input starting at %s", s.position())
	}
	return val, nil
}

// Skipping returns a parser that skips runes matching pred before each terminal
// parser (Equal, EqualUnlessFollowedBy, Match, One, Regexp, Word and While).
// If f is nil, no input is skipped.
func Skipping[T any](pred func(rune) bool, p Parser[T]) Parser[T] {
	return func(s *State) T {
		defer func(f func(rune) bool) {
			s.skipPred = f
		}(s.skipPred)
		s.skipPred = pred
		return p(s)
	}
}

// Equal returns a parser that matches the given string exactly.
// Note that Equal will succeed even if the string is part of a larger
// word. For example, Equal("foo") succeeds on "food", matching only "foo"
// and leaving the "d" unconsumed. To match only the word "foo", use Word.
func Equal(e string) Parser[string] {
	return Match(strconv.Quote(e), func(s string) int {
		if strings.HasPrefix(s, e) {
			return len(e)
		}
		return -1
	})
}

// EqualUnlessFollowedBy returns a parser that matches e, but only if it is not
// followed by a rune for which pred returns true.
func EqualUnlessFollowedBy(e string, pred func(rune) bool) Parser[string] {
	return Match(strconv.Quote(e), func(s string) int {
		if len(s) < len(e) {
			return -1
		}
		if s[:len(e)] != e {
			return -1
		}
		if len(s) == len(e) {
			return len(e)
		}
		r, _ := utf8.DecodeRuneInString(s[len(e):])
		if pred(r) {
			return -1
		}
		return len(e)
	})
}

// Word parses the given string, provided it is followed by the end of input or
// a non-word character.
//
// Word(w) is equivalent to EqualUnlessFollowedBy(w, isWordChar)
// where isWordChar returns true for underscore and for unicode letters and digits.
func Word(w string) Parser[string] {
	return EqualUnlessFollowedBy(w, isWordChar)
}

func isWordChar(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// One matches a single rune for which pred returns true. The name
// is only used for error messages.
func One(name string, pred func(rune) bool) Parser[string] {
	return Match(name, func(s string) int {
		if !utf8.FullRuneInString(s) {
			return -1
		}
		r, size := utf8.DecodeRuneInString(s)
		if pred(r) {
			return size
		}
		return -1
	})
}

// While returns a parser that parses a non-empty sequence of runes for which
// pred is true. The name is only used for error messages.
func While(name string, pred func(rune) bool) Parser[string] {
	return Match(name, func(s string) int {
		for i, r := range s {
			if !pred(r) {
				if i == 0 {
					return -1
				}
				return i
			}
		}
		return len(s)
	})
}

// Regexp returns a parser that matches input using the given regular expression.
// The name is used for error messages.
func Regexp(name, sre string) Parser[string] {
	re := regexp.MustCompile("^" + sre)
	return Match(name, func(input string) int {
		loc := re.FindStringIndex(input)
		if loc == nil {
			return -1
		}
		return loc[1]
	})
}

// Match returns a parser that calls calls f on its input. f should return the
// length of the matching string, or a negative number if there is no match.
// If f returns a non-negative integer, Match consumes that many bytes
// of input and succeeds. Otherwise, it fails.
// The name is only used for error messages.
func Match(name string, f func(string) int) Parser[string] {
	return func(s *State) string {
		s.skip()
		matchLen := f(s.input[s.pos:])
		if matchLen < 0 {
			Failf("expected %s at %s", name, s.position())
		}
		start := s.pos
		s.pos += matchLen
		return s.input[start:s.pos]
	}
}

func Trace[T any](name string, p Parser[T]) Parser[T] {
	return func(s *State) T {
		fmt.Printf("> %s on %q\n", name, s.input[s.pos:])
		t := p(s)
		fmt.Printf("< %s: %v\n", name, t)
		return t
	}
}

// And returns a parser that
// TODO XXXXXXXXXXXXXXXX
// invokes its argument parsers in succession,
// and fails as soon as one of the parsers fails.
// The parser returns a slice of the argument parsers' XXXXXXXXXXXXXXXX non-nil XXXX values.
func And[T any](parsers ...Parser[T]) Parser[[]T] {
	return func(s *State) []T {
		var ts []T
		for _, p := range parsers {
			ts = append(ts, p(s))
		}
		return ts
	}
}

func And2[T1, T2, R any](p1 Parser[T1], p2 Parser[T2], do func(T1, T2) R) Parser[R] {
	return func(s *State) R {
		return do(p1(s), p2(s))
	}
}

func And3[T1, T2, T3, R any](p1 Parser[T1], p2 Parser[T2], p3 Parser[T3], do func(T1, T2, T3) R) Parser[R] {
	return func(s *State) R {
		return do(p1(s), p2(s), p3(s))
	}
}

func And4[T1, T2, T3, T4, R any](p1 Parser[T1], p2 Parser[T2], p3 Parser[T3], p4 Parser[T4], do func(T1, T2, T3, T4) R) Parser[R] {
	return func(s *State) R {
		return do(p1(s), p2(s), p3(s), p4(s))
	}
}

func And5[T1, T2, T3, T4, T5, R any](p1 Parser[T1], p2 Parser[T2], p3 Parser[T3], p4 Parser[T4], p5 Parser[T5], do func(T1, T2, T3, T4, T5) R) Parser[R] {
	return func(s *State) R {
		return do(p1(s), p2(s), p3(s), p4(s), p5(s))
	}
}

// Or tries each of its argument parsers in turn on the same input, succeeding
// as soon as the first succeeds and failing if they all fail.
// The Commit parser modifies that behavior: if an argument parser calls Commit,
// then Or fails as soon as that parser fails instead of trying the next argument.
func Or[T any](parsers ...Parser[T]) Parser[T] {
	return func(s *State) T {
		start := s.pos
		defer func(c bool) { s.committed = c }(s.committed)
		s.committed = false

		for _, p := range parsers {
			val, err := Catch(p, s)
			if err != nil && s.committed {
				Fail(err)
			}
			if err == nil {
				return val
			}
			s.pos = start
		}
		Failf("parse failed at %s", s.position())
		panic("unreachable")
	}
}

// Empty parses the empty input and returns the zero value.
func Empty[T any]() Parser[T] {
	return func(*State) (z T) { return z }
}

// Cut causes Or to stop trying alternatives on an error.
// See Or's documentation for more.
func Cut[T any](p Parser[T]) Parser[T] {
	return func(s *State) T {
		v := p(s)
		s.committed = true
		return v
	}
}

var (
	// Int parses signed decimal integers.
	// It accepts only the ASCII digits 0 through 9.
	// Its value is an int64.
	Int = Do(
		Regexp("integer", `[+-]?[0-9]+`),
		func(s string) int64 {
			i, err := strconv.ParseInt(s, 10, 64)
			if err != nil {
				Fail(err)
			}
			return i
		})

	// Float parses and returns a float64, using standard decimal notation.
	Float = Do(
		Regexp("floating-point number", `[+-]?(\d+(\.\d*)?([Ee][+-]?\d+)?|\d*\.\d+([Ee][+-]?\d+)?)`),
		func(s string) float64 {
			f, err := strconv.ParseFloat(s, 64)
			if err != nil {
				Fail(err)
			}
			return f
		})
)

// Optional parses either what p parses, or nothing.
// If p succeeds, a pointer to p's result it returned.
// Otherwise, the parse value is nil.
func Optional[T any](p Parser[T]) Parser[*T] {
	if _, err := p.Parse(""); err == nil {
		panic("Optional called with a parser that accepts the empty string; you don't need Optional")
	}
	return Or(
		Do(p, func(v T) *T { return &v }),
		Empty[*T]())
}

// Do returns a parser that parses some tokens using p. If p succeeds, then f is
// called with the value of p and its return value is the value of Do.
func Do[T, U any](p Parser[T], f func(T) U) Parser[U] {
	return func(s *State) U {
		return f(p(s))
	}
}

// Then is like Do, but it passes the parse state to the function, allowing
// it to call a parser knowing the value of the previous parser.
func Then[T, U any](p Parser[T], f func(t T, s *State) U) Parser[U] {
	return func(s *State) U {
		return f(p(s), s)
	}
}

// Repeat calls p repeatedly until it fails.
func RepeatRecursive[T any](p Parser[T]) Parser[[]T] {
	if _, err := p.Parse(""); err == nil {
		panic("Repeat called with a parser that accepts the empty string; that will lead to a stack overflow")
	}
	return Do(repeat(p), reverse[T])
}

func reverse[T any](ts []T) []T {
	for i := len(ts)/2 - 1; i >= 0; i-- {
		j := len(ts) - i - 1
		ts[i], ts[j] = ts[j], ts[i]
	}
	return ts
}

// repeat produces a list of values parsed by p in reverse order.
func repeat[T any](p Parser[T]) Parser[[]T] {
	// We can't write
	//  Or(And(p, repeat(p)), Empty)
	// as we would like. Go is applicative-order, so the recursive call to Repeat happens
	// immediately and we have infinite recursion. We must delay the recursion using Ptr.
	var r Parser[[]T]
	r = Or(
		And2(p, Ptr(&r), func(t T, ts []T) []T { return append(ts, t) }),
		Empty[[]T](),
	)
	return r
}

// List returns a parser that parses a non-empty list of items separated by sep.
// The parser returns a slice of the items' values, ignoring the seps' values.
func ListRecursive[T, U any](item Parser[T], sep Parser[U]) Parser[[]T] {
	return Do(
		And2(
			item,
			repeat(And2(sep, item, func(_ U, t T) T { return t })),
			func(t T, ts []T) []T {
				return append(ts, t)
			}),
		reverse[T])
}

func LeftFold[T, U any](init Parser[T], rep Parser[U], fold func(t T, u U) T) Parser[T] {
	opt := Optional(rep)
	return Then(init, func(t T, s *State) T {
		for {
			pu := opt(s)
			if pu == nil {
				return t
			}
			t = fold(t, *pu)
		}
	})
}

func LeftFold2[T, U any](init T, rep Parser[U], fold func(t T, u U) T) Parser[T] {
	opt := Optional(rep)
	t := init
	return func(s *State) T {
		for {
			pu := opt(s)
			if pu == nil {
				return t
			}
			t = fold(t, *pu)
		}
	}
}

func append1[T any](ts []T, t T) []T {
	return append(ts, t)
}

func Repeat1[T any](p Parser[T]) Parser[[]T] {
	return LeftFold(
		Do(p, func(t T) []T { return []T{t} }),
		p,
		append1[T])
}

func Repeat[T any](p Parser[T]) Parser[[]T] {
	return Or(Repeat1(p), Empty[[]T]())
}

func List[T, U any](item Parser[T], sep Parser[U]) Parser[[]T] {
	return LeftFold(
		Do(item, func(t T) []T { return []T{t} }),
		And2(sep, item, func(_ U, t T) T { return t }),
		append1[T])
}

// Ptr returns a parser that invokes *p.
// It is useful for creating recursive parsers.
// See the calculator example for a typical use.
func Ptr[T any](p *Parser[T]) Parser[T] {
	return func(s *State) T {
		return (*p)(s)
	}
}

type fail struct {
	err error
}

func Catch[T any](p Parser[T], s *State) (r T, err error) {
	defer func() {
		if x := recover(); x != nil {
			if f, ok := x.(fail); ok {
				var z T
				r = z
				err = f.err
				return
			}
			panic(x)
		}
	}()
	return p(s), nil
}

func Fail(err error) {
	panic(fail{err})
}

func Failf(format string, args ...interface{}) {
	Fail(fmt.Errorf(format, args...))
}
