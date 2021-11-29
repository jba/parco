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
type Parser[T any] func(*State) (T, error)

// Parse uses the given Parser to parse the tokens. The value argument is put
// the Value field of the State that is passed to user-defined functions.
// By default, whitespace in the input is skipped. Call Skipping to change
// that behavior.
func (p Parser[T]) Parse(input string) (z T, _ error) {
	s := newState(input)
	val, err := p(s)
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
	return func(s *State) (T, error) {
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
// is used for error messages.
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
// pred is true. The name is used for error messages.
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
	return Match(name, func(s string) int {
		loc := re.FindStringIndex(s)
		if loc == nil {
			return -1
		}
		return loc[1]
	})
}

// Match returns a parser that calls calls f on its input.
// f should return the length of the matching string, or -1
// if there is no match. The name is used for error messages.
func Match(name string, f func(string) int) Parser[string] {
	return func(s *State) (string, error) {
		s.skip()
		matchLen := f(s.input[s.pos:])
		if matchLen < 0 {
			return "", fmt.Errorf("expected %s at %s", name, s.position())
		}
		start := s.pos
		s.pos += matchLen
		return s.input[start:s.pos], nil
	}
}

// And returns a parser that
// TODO XXXXXXXXXXXXXXXX
// invokes its argument parsers in succession,
// and fails as soon as one of the parsers fails.
// The parser returns a slice of the argument parsers' XXXXXXXXXXXXXXXX non-nil XXXX values.
func And[T any](parsers ...Parser[T]) Parser[[]T] {
	return func(s *State) (z []T, _ error) {
		var ts []T
		for _, p := range parsers {
			val, err := p(s)
			if err != nil {
				return z, err
			}
			ts = append(ts, val)
		}
		return ts, nil
	}
}

// type Pair[T, U any] struct {
// 	First  T
// 	Second U
// }

func And2[T1, T2, R any](p1 Parser[T1], p2 Parser[T2], do func(T1, T2) R) Parser[R] {
	return func(s *State) (z R, _ error) {
		return doAnd2(s, p1, p2, func(s *State, t1 T1, t2 T2) (R, error) {
			return do(t1, t2), nil
		})
	}
}

func doAnd2[T1, T2, R any](s *State, p1 Parser[T1], p2 Parser[T2], do func(*State, T1, T2) (R, error)) (z R, err error) {
	t1, err := p1(s)
	if err != nil {
		return z, err
	}
	t2, err := p2(s)
	if err != nil {
		return z, err
	}
	return do(s, t1, t2)
}

func And3[T1, T2, T3, R any](p1 Parser[T1], p2 Parser[T2], p3 Parser[T3], do func(T1, T2, T3) R) Parser[R] {
	return func(s *State) (z R, _ error) {
		return doAnd2(s, p1, p2, func(s *State, t1 T1, t2 T2) (z R, err error) {
			t3, err := p3(s)
			if err != nil {
				return z, err
			}
			return do(t1, t2, t3), nil
		})
	}
}

func And4[T1, T2, T3, T4, R any](p1 Parser[T1], p2 Parser[T2], p3 Parser[T3], p4 Parser[T4], do func(T1, T2, T3, T4) R) Parser[R] {
	return func(s *State) (z R, _ error) {
		return doAnd2(s, p1, p2, func(s *State, t1 T1, t2 T2) (z R, err error) {
			return doAnd2(s, p3, p4, func(s *State, t3 T3, t4 T4) (z R, err error) {
				return do(t1, t2, t3, t4), nil
			})
		})
	}
}

// Or tries each of its argument parsers in turn on the same input, succeeding
// as soon as the first succeeds and failing if they all fail.
// The Commit parser modifies that behavior: if an argument parser calls Commit,
// then Or fails as soon as that parser fails instead of trying the next argument.
func Or[T any](parsers ...Parser[T]) Parser[T] {
	return func(s *State) (z T, _ error) {
		start := s.pos
		defer func(c bool) { s.committed = c }(s.committed)
		s.committed = false

		for _, p := range parsers {
			val, err := p(s)
			if err != nil && s.committed {
				return z, err
			}
			if err == nil {
				return val, nil
			}
			s.pos = start
		}
		return z, fmt.Errorf("parse failed at %s", s.position())
	}
}

// Empty parses the empty input and returns the zero value.
func Empty[T any]() Parser[T] {
	return func(*State) (z T, _ error) { return z, nil }
}

// Cut causes Or to stop trying alternatives on an error.
// See Or's documentation for more.
func Cut[T any](p Parser[T]) Parser[T] {
	return func(s *State) (z T, _ error) {
		v, err := p(s)
		if err != nil {
			return z, err
		}
		s.committed = true
		return v, nil
	}
}

var (
	// Int parses signed decimal integers.
	// It accepts only the ASCII digits 0 through 9.
	// Its value is an int64.
	Int = DoErr(
		Regexp("integer", `[+-]?[0-9]+`),
		func(s string) (int64, error) { return strconv.ParseInt(s, 10, 64) })

	// Float parses and returns a float64, using standard decimal notation.
	Float = DoErr(
		Regexp("floating-point number", `[+-]?(\d+(\.\d*)?([Ee][+-]?\d+)?|\d*\.\d+([Ee][+-]?\d+)?)`),
		func(s string) (float64, error) { return strconv.ParseFloat(s, 64) })
)

// Optional parses either what p parses, or nothing.
// In the latter case, the parse value is nil.
func Optional[T any](p Parser[T]) Parser[*T] {
	if _, err := p.Parse(""); err == nil {
		panic("Optional called with a parser that accepts the empty string; you don't need Optional")
	}
	return Or(
		Do(p, func(v T) *T { return &v }),
		Empty[*T]())
}

// Repeat calls p repeatedly until it fails.
func Repeat[T any](p Parser[T]) Parser[[]T] {
	if _, err := p.Parse(""); err == nil {
		panic("Repeat called with a parser that accepts the empty string; that will lead to a stack overflow")
	}

	// We can't write
	//  Or(And(p, Repeat(p)), Empty)
	// as we would like. Go is applicative-order, so the recursive call to Repeat happens
	// immediately and we have infinite recursion. We must delay the recursion.
	return Or(
		And2(
			p,
			func(s *State) ([]T, error) { return Repeat(p)(s) },
			func(v1 T, v2 []T) []T {
				return append([]T{v1}, v2...)
			}),
		Empty[[]T]())
}

// List returns a parser that parses a non-empty list of items separated by sep.
// The parser returns a slice of the items' values, ignoring the seps' values.
func List[T, U any](item Parser[T], sep Parser[U]) Parser[[]T] {
	return And2(
		item,
		Repeat(And2(sep, item, func(_ U, t T) T { return t })),
		func(t T, ts []T) []T {
			return append([]T{t}, ts...)
		})
}

// Do returns a parser that parses some tokens using p. If p succeeds, then f is
// called with the value of p and its return value is the value of Do.
func Do[T, U any](p Parser[T], f func(T) U) Parser[U] {
	return DoErr(p, func(t T) (U, error) { return f(t), nil })
}

// DoErr is like Do, but the function may return an error, which terminates
// the parse if it is non-nil.
func DoErr[T, U any](p Parser[T], f func(T) (U, error)) Parser[U] {
	return func(s *State) (z U, _ error) {
		t, err := p(s)
		if err != nil {
			return z, err
		}
		return f(t)
	}
}

// Ptr returns a parser that invokes *p.
// It is useful for creating recursive parsers.
// See the calculator example for a typical use.
func Ptr[T any](p *Parser[T]) Parser[T] {
	return func(s *State) (T, error) {
		return (*p)(s)
	}
}
