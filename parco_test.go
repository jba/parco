// Copyright 2021 Jonathan Amsterdam.

package parco

import (
	"strings"
	"testing"
	"unicode"

	"github.com/google/go-cmp/cmp"
)

func TestWord(t *testing.T) {
	p := Word("foo")
	for _, test := range []struct {
		in      string
		want    string
		wantErr string
	}{
		{"", "", `expected "foo"`},
		{"foo", "foo", ""},
		{"food", "", `expected "foo"`},
		{" \t\n foo", "foo", ""},
		{"foo bar", "foo", ""},
		{"foo+bar", "foo", ""},
	} {
		got, gotErr := p(newState(test.in))
		if got != test.want {
			t.Errorf("%q: got (%v, %v), want %v", test.in, got, gotErr, test.want)
			continue
		}
		if gotErr == nil && test.wantErr != "" {
			t.Errorf("%q: succeeded, but want error containing %q", test.in, test.wantErr)
			continue
		}
		if gotErr != nil {
			if test.wantErr == "" {
				t.Errorf("%q: got %v, want success", test.in, gotErr)
				continue
			}
			if !strings.Contains(gotErr.Error(), test.wantErr) {
				t.Errorf("%q: got `%v`, want error containing %q", test.in, gotErr, test.wantErr)
			}
		}
	}
}

type test[T any] struct {
	name    string
	p       Parser[T]
	in      string
	want    T
	wantErr string // error string must contain this
}

func (s test[T]) run(t *testing.T) {
	t.Run(s.name, func(t *testing.T) {
		got, err := s.p.Parse(s.in)
		if err != nil {
			if s.wantErr != "" {
				if !strings.Contains(err.Error(), s.wantErr) {
					t.Fatalf("got '%v', wanted error containing %q", err, s.wantErr)
				}
				return
			}
			t.Fatalf("got '%v', want success", err)
		}
		if s.wantErr != "" {
			t.Fatalf("got success, want error containing %q", s.wantErr)
		}
		if diff := cmp.Diff(s.want, got); diff != "" {
			t.Errorf("mismatch (-want, +got)\n%s", diff)
		}
	})
}

func TestParse(t *testing.T) {
	andComma := func(ps ...Parser[string]) Parser[string] {
		return Do(And(ps...), func(v []string) string {
			return strings.Join(v, ",")
		})
	}

	for _, test := range []test[string]{
		{
			name: "Empty",
			p:    Empty[string](),
			in:   "",
			want: "",
		},
		{
			name:    "unconsumed",
			p:       Empty[string](),
			in:      "x",
			wantErr: "unconsumed input",
		},
		{
			name: "Equal",
			p:    Equal("foo"),
			in:   "foo",
			want: "foo",
		},
		{
			name:    "Equal fail",
			p:       Equal("foo"),
			in:      "bar",
			wantErr: `expected "foo"`,
		},
		{
			name: "Word",
			p:    Word("foo"),
			in:   "foo",
			want: "foo",
		},
		{
			name: "Word with space",
			p:    Word("foo"),
			in:   "  \t\n foo",
			want: "foo",
		},
		{
			name:    "Word partial",
			p:       Word("foo"),
			in:      "foob",
			wantErr: `expected "foo"`,
		},
		{
			name: "One",
			p:    One("digit", unicode.IsDigit),
			in:   "3",
			want: "3",
		},
		{
			name:    "One fail",
			p:       One("digit", unicode.IsDigit),
			in:      "x",
			wantErr: "expected digit",
		},
		{
			name: "While",
			p:    While("letters", unicode.IsLetter),
			in:   "abc",
			want: "abc",
		},
		{
			name:    "While fail",
			p:       While("letters", unicode.IsLetter),
			in:      "123",
			wantErr: "expected letters",
		},
		{
			name: "Regexp",
			p:    Regexp("abcs", `(abc)+`),
			in:   "abcabcabc  ",
			want: "abcabcabc",
		},
		{
			name: "Do",
			p:    Do(Word("foo"), func(v string) string { return strings.ToUpper(v) }),
			in:   "foo",
			want: "FOO",
		},
		{
			name: "Or a",
			p:    Or(Word("a"), Word("b")),
			in:   "a",
			want: "a",
		},
		{
			name: "Or b",
			p:    Or(Word("a"), Word("b")),
			in:   "b",
			want: "b",
		},
		{
			name:    "Or fail",
			p:       Or(Word("a"), Word("b")),
			in:      "c",
			wantErr: `parse failed at index 0 ("c")`,
		},
	} {
		test.run(t)
	}

	for _, test := range []test[[]string]{
		{
			name: "Equal2",
			p:    And(Equal("foo"), Equal("d")),
			in:   "food",
			want: []string{"foo", "d"},
		},
		{
			name: "Equal3",
			p:    And(Equal("foo"), Equal("d")),
			in:   "  foo\td\n",
			want: []string{"foo", "d"},
		},
		{
			name: "While2",
			p:    And(While("letters", unicode.IsLetter), One("digit", unicode.IsDigit)),
			in:   "abc3",
			want: []string{"abc", "3"},
		},
		{
			name: "Regexp2",
			p:    And(Regexp("abcs", `(abc)+`), Equal("abd")),
			in:   "abcabcabd",
			want: []string{"abcabc", "abd"},
		},
		{
			name: "And",
			p:    And(Word("foo"), Word("bar")),
			in:   "foo bar",
			want: []string{"foo", "bar"},
		},
		{
			name: "nested And",
			p: And(
				Word("foo"),
				andComma(Word("bar"), Word("baz")),
				Word("boo")),
			in:   "foo bar baz boo",
			want: []string{"foo", "bar,baz", "boo"},
		},
		{
			name: "Or empty",
			p:    And(Or(Word("a"), Empty[string]()), Word("c")),
			in:   "c",
			want: []string{"", "c"},
		},
		{
			name: "Repeat empty",
			p:    Repeat(Word("x")),
			in:   "",
			want: nil,
		},
		{
			name: "Repeat 1",
			p:    Repeat(Word("x")),
			in:   "x",
			want: []string{"x"},
		},
		{
			name: "Repeat 2",
			p:    Repeat(Word("x")),
			in:   "x x",
			want: []string{"x", "x"},
		},
		{
			name: "List",
			p:    List(Regexp("id", `[a-z]+`), Equal(",")),
			in:   "a, b ,c , d",
			want: []string{"a", "b", "c", "d"},
		},
		{
			name: "without Cut",
			p:    Or(And(Word("a"), Word("b")), And(Word("c"), Word("d"))),
			in:   "a d",
			// We'd like "expected b",but we get this instead:
			wantErr: `parse failed`,
		},
		{
			name:    "Cut",
			p:       Or(And(Cut(Word("a")), Word("b")), And(Word("c"), Word("d"))),
			in:      "a d",
			wantErr: `expected "b"`,
		},
		{
			name: "nested Cut",
			// Inner cut doesn't affect outer Or.
			p: Or(
				And(
					Or(
						andComma(Cut(Word("a")), Word("b")),
						Word("c")),
					Word("d")),
				And(Word("e"), Word("f"))),
			in:      "f",
			wantErr: "parse failed",
		},
		{
			name: "not Skipping",
			p:    And(Word("notabs"), While("any", func(rune) bool { return true })),
			in:   "notabs  \t\tx",
			want: []string{"notabs", "x"},
		},
		{
			name: "Skipping",
			p: And(
				Word("notabs"),
				Skipping(func(r rune) bool { return r == ' ' },
					While("any", func(rune) bool { return true }))),
			in:   "notabs  \t\tx",
			want: []string{"notabs", "\t\tx"},
		},
		{
			name: "Skipping nil",
			p: And(
				Word("notabs"),
				Skipping(nil,
					While("any", func(rune) bool { return true }))),
			in:   "notabs  \t\tx",
			want: []string{"notabs", "  \t\tx"},
		},
	} {
		test.run(t)
	}

	test[[][]string]{
		name: "Repeat multi",
		p:    Repeat(And(One("letter", unicode.IsLetter), One("digit", unicode.IsDigit))),
		in:   "a1c3e 5 g 7",
		want: [][]string{[]string{"a", "1"}, []string{"c", "3"}, []string{"e", "5"}, []string{"g", "7"}},
	}.run(t)
}

func TestInt(t *testing.T) {
	for _, test := range []struct {
		in   string
		want int64
	}{
		{"1", 1},
		{"+23", 23},
		{"-923", -923},
		{"999999", 999999},
	} {
		got, err := Int.Parse(test.in)
		if err != nil {
			t.Fatal(err)
		}
		if got != test.want {
			t.Errorf("got %d, want %d", got, test.want)
		}
	}
}

func TestFloat(t *testing.T) {
	for _, test := range []struct {
		in   string
		want float64
	}{
		{"1", 1},
		{"+23", +23},
		{"-923", -923},
		{"999999", 999999},
		{"1e3", 1e3},
		{"-23.875", -23.875},
		{"-0.", -0.},
		{"-1.e+3", -1.e+3},
		{".25", .25},
		{".5E11", .5e11},
		{"3.e5", 3.e5},
		{"1e-3", 1e-3},
	} {
		got, err := Float.Parse(test.in)
		if err != nil {
			t.Fatalf("%q: %v", test.in, err)
		}
		if got != test.want {
			t.Errorf("%q: got %g, want %g", test.in, got, test.want)
		}
	}
	for _, in := range []string{".", "e23", "--1", ".e1"} {
		_, err := Float.Parse(in)
		want := "expected floating-point number"
		if err == nil || !strings.HasPrefix(err.Error(), want) {
			t.Errorf("%q: got `%v`, want error starting %q", in, err, want)
		}
	}
}

func TestParseQuery(t *testing.T) {
	type query struct {
		selects []string
		coll    string
		limit   int64
	}

	ident := Regexp("id", `[_\pL][_\pL\p{Nd}]*`)

	selectClause := And2(
		Word("select"),
		Or(
			Do(Equal("*"), func(string) []string { return nil }),
			List(ident, Equal(","))),
		func(_ string, v []string) *query {
			return &query{selects: v}
		})

	fromClause := Do(
		And(Word("from"), ident),
		func(v []string) string {
			return v[1]
		})

	limitClause := And2(
		Cut(Word("limit")), Int,
		func(_ string, v int64) int64 { return v })

	p := And2(
		And2(selectClause, fromClause, func(q *query, s string) *query {
			q.coll = s
			return q
		}),
		Optional(limitClause),
		func(q *query, pi *int64) *query {
			if pi != nil {
				q.limit = *pi
			}
			return q
		})

	for _, test := range []struct {
		in   string
		want query
		err  string
	}{
		{
			in:   "select * from cities",
			want: query{selects: nil, coll: "cities"},
		},
		{
			in:   "select a , b , c from d",
			want: query{selects: []string{"a", "b", "c"}, coll: "d"},
		},
		{
			in:   "select * from cities limit 5",
			want: query{selects: nil, coll: "cities", limit: 5},
		},
		{
			in:  "select from x",
			err: `expected "from"`,
		},
		{
			in:  "select a , from x",
			err: `expected "from"`,
		},
		{
			in:  "select * from cities and more",
			err: `unconsumed input`,
		},
		{
			in:  "query",
			err: `expected "select"`,
		},
		{
			in:  "select * from x limit b",
			err: `expected int`,
		},
	} {
		got, err := p.Parse(test.in)
		if err == nil {
			if test.err != "" {
				t.Errorf("%q: got success, want error", test.in)
			} else if diff := cmp.Diff(&test.want, got, cmp.AllowUnexported(query{})); diff != "" {
				t.Errorf("%q: mismatch (-want, +got)\n%s", test.in, diff)
			}
		} else {
			if test.err == "" {
				t.Errorf("%q: got %v, want success", test.in, err)
			} else if g := err.Error(); !strings.Contains(g, test.err) {
				t.Errorf("%q, error:\ngot:  %s\nwant: %s", test.in, g, test.err)
			}
		}
	}
}

func TestPanicOnEmpty(t *testing.T) {
	// Verify that Repeat and Optional panic if the parser it is given succeeds on the empty string.
	t.Run("Repeat", func(t *testing.T) {
		defer func() {
			if r := recover(); r == nil {
				t.Fatal("wanted panic, didn't get one")
			}
		}()
		Repeat(Regexp("digits", `\d*`))
	})
	t.Run("Optional", func(t *testing.T) {
		defer func() {
			if r := recover(); r == nil {
				t.Fatal("wanted panic, didn't get one")
			}
		}()
		Optional(Regexp("digits", `\d*`))
	})
}
