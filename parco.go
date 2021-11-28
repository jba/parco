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

     val, err := p.Parse("the big dog", nil)

The value of this parse will be a slice of the input words:

     ["the", "big", "dog"]


Actions

Every Parser returns a Value, which can be anything. (Value is an alias for
interface{}.) The value of the top-level parser is returned by Parser.Parse,
but you can modify or act upon any parser's value by associating an action
with it. To do so, call its Do method with a function that takes a Value and
returns (Value, error). The argument is the parser's value, and the returned
value replaces it. Returning an error immediately fails the entire parse.

For example, to replace consecutive  "big"s in the input string, we could
modify the above parser like so:

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

then the parser will not backtrack past the cut, and the input "limit x" produces
the error "expected integer".

You will usually want to add Cut after the first token of a particular parsing
choice, because most modern formal languages are designed to be parsed by
looking ahead only a single token. But you can put it anywhere you like.
*/
package parco

import (
	"fmt"
	"reflect"
	"regexp"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// A Value is something returned from a parse.
type Value = interface{}

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
type Parser func(*State) (Value, error)

// Parse uses the given Parser to parse the tokens. The value argument is put
// the Value field of the State that is passed to user-defined functions.
// By default, whitespace in the input is skipped. Call Skipping to change
// that behavior.
func (p Parser) Parse(input string) (Value, error) {
	s := newState(input)
	val, err := p(s)
	if err != nil {
		return nil, err
	}
	s.skip()
	if s.pos != len(s.input) {
		return nil, fmt.Errorf("unconsumed input starting at %s", s.position())
	}
	return val, nil
}

// Skipping returns a parser that skips runes matching pred before each terminal
// parser (Equal, EqualUnlessFollowedBy, Match, One, Regexp, Word and While).
// If f is nil, no input is skipped.
func Skipping(pred func(rune) bool, p Parser) Parser {
	return func(s *State) (Value, error) {
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
func Equal(e string) Parser {
	return Match(strconv.Quote(e), func(s string) int {
		if strings.HasPrefix(s, e) {
			return len(e)
		}
		return -1
	})
}

// EqualUnlessFollowedBy returns a parser that matches e, but only if it is not
// followed by a rune for which pred returns true.
func EqualUnlessFollowedBy(e string, pred func(rune) bool) Parser {
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
func Word(w string) Parser {
	return EqualUnlessFollowedBy(w, isWordChar)
}

func isWordChar(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}

// One matches a single rune for which pred returns true. The name
// is used for error messages.
func One(name string, pred func(rune) bool) Parser {
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
func While(name string, pred func(rune) bool) Parser {
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
func Regexp(name, sre string) Parser {
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
func Match(name string, f func(string) int) Parser {
	return func(s *State) (Value, error) {
		s.skip()
		matchLen := f(s.input[s.pos:])
		if matchLen < 0 {
			return nil, fmt.Errorf("expected %s at %s", name, s.position())
		}
		start := s.pos
		s.pos += matchLen
		return s.input[start:s.pos], nil
	}
}

// And returns a parser that invokes its argument parsers in succession,
// and fails as soon as one of the parsers fails.
// The parser returns a slice of the argument parsers' non-nil values.
func And(parsers ...Parser) Parser {
	return func(s *State) (Value, error) {
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
// The Commit parser modifies that behavior: if an argument parser calls Commit,
// then Or fails as soon as that parser fails instead of trying the next argument.
func Or(parsers ...Parser) Parser {
	return func(s *State) (Value, error) {
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
		return nil, fmt.Errorf("parse failed at %s", s.position())
	}
}

var (
	// Empty parses the empty input and returns nil.
	Empty Parser = func(*State) (Value, error) { return nil, nil }

	// Cut causes Or to stop trying alternatives on an error.
	// See Or's documentation for more.
	Cut Parser = func(s *State) (Value, error) { s.committed = true; return nil, nil }

	// Int parses signed decimal integers.
	// It accepts only the ASCII digits 0 through 9.
	// Its value is an int64.
	Int = Regexp("integer", `[+-]?[0-9]+`).Do(func(v Value) (Value, error) {
		return strconv.ParseInt(v.(string), 10, 64)
	})

	// Float parses and returns a float64, using standard decimal notation.
	Float = Regexp("floating-point number",
		`[+-]?(\d+(\.\d*)?([Ee][+-]?\d+)?|\d*\.\d+([Ee][+-]?\d+)?)`).Do(
		func(v Value) (Value, error) {
			return strconv.ParseFloat(v.(string), 64)
		})
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
		And(p, func(s *State) (Value, error) { return Repeat(p)(s) }),
		Empty).Do(func(v Value) (Value, error) {
		// v is either nil (from Empty),
		// or []Value{vp} where vp is the value of p, if the nested Repeat is Empty,
		// or []Value{vp, vr} where vr is the value of the nested Repeat.
		if v == nil {
			return nil, nil
		}
		vs := v.([]Value)
		if len(vs) == 1 {
			return v, nil
		}
		return append([]Value{vs[0]}, vs[1].([]Value)...), nil
	})
}

// List returns a parser that parses a non-empty list of items separate by sep.
// The parser returns a slice of the items' values, ignoring the seps' values.
func List(item, sep Parser) Parser {
	return And(item, Repeat(
		And(sep.Do(func(Value) (Value, error) { return nil, nil }),
			item))).
		Do(func(v Value) (Value, error) {
			return flatten(v.([]Value)), nil
		})
}

func flatten(vs []Value) []Value {
	var r []Value
	for _, v := range vs {
		if s, ok := v.([]Value); ok {
			r = append(r, flatten(s)...)
		} else {
			r = append(r, v)
		}
	}
	return r
}

// Do first parses some tokens using p. If p succeeds, then it calls f, which
// must be a function, with the parse state and the value of p. The function's
// return value is the value of Do. If the function returns an error, the parse
// fails immediately.
//
// Do panics if its argument does not have one
// of these signatures:
//
//   func(Value) (Value, error)
// This is the most general signature. The signatures below are described by how
// they map into this one.
//
//   func(Value) Value
// The returned error is nil.
//
//   func(Value)
// The returned value is the argument value and the return error is nil.
//
//   func([]Value) (Value, error)
//   func([]Value) Value
//   func([]Value)
// These panic if the argument is not a slice. Otherwise they behave like
// their counterparts above.
func (p Parser) Do(f interface{}) Parser {
	return func(s *State) (Value, error) {
		val, err := p(s)
		if err != nil {
			return nil, err
		}
		return convertDoFunc(f)(val)
	}
}

var (
	valueSliceType = reflect.TypeOf([]Value{})
	valueType      = valueSliceType.Elem()
	errorType      = reflect.TypeOf([]error{}).Elem()
)

func convertDoFunc(f interface{}) func(Value) (Value, error) {
	t := reflect.TypeOf(f)
	if t.Kind() != reflect.Func {
		panic("argument to Do is not a function")
	}
	if t.NumIn() != 1 {
		panic("argument to Do must be a function with one argument")
	}
	sliceArg := t.In(0) == valueSliceType
	if !sliceArg && t.In(0) != valueType {
		panic("argument to Do must be a function whose argument is a Value or []Value")
	}
	if t.NumOut() >= 1 && t.Out(0) != valueType {
		panic("argument to Do must be a function whose first return value is parco.Value")
	}
	if t.NumOut() == 2 && t.Out(1) != errorType {
		panic("argument to Do must be a function whose second return value is error")
	}
	switch t.NumOut() {
	case 0:
		if sliceArg {
			g := f.(func([]Value))
			return func(v Value) (Value, error) {
				g(v.([]Value))
				return v, nil
			}
		}
		g := f.(func(Value))
		return func(v Value) (Value, error) {
			g(v)
			return v, nil
		}

	case 1:
		if sliceArg {
			g := f.(func([]Value) Value)
			return func(v Value) (Value, error) {
				return g(v.([]Value)), nil
			}
		}
		g := f.(func(Value) Value)
		return func(v Value) (Value, error) {
			return g(v), nil
		}

	case 2:
		if sliceArg {
			g := f.(func([]Value) (Value, error))
			return func(v Value) (Value, error) {
				return g(v.([]Value))
			}
		}
		return f.(func(Value) (Value, error))

	default:
		panic("argument to Do can have at most two return values")
	}
}

// Ptr returns a parser that invokes *p.
// It is useful for creating recursive parsers.
// See the calculator example for a typical use.
func Ptr(p *Parser) Parser {
	return func(s *State) (Value, error) {
		return (*p)(s)
	}
}
