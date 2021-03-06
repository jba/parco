// Copyright 2021 Jonathan Amsterdam.

package parco

import (
	"io"
	"reflect"
	"strings"
	"testing"
	"unicode"

	"github.com/google/go-cmp/cmp"
)

func TestWord(t *testing.T) {
	p := Word("foo")
	for _, test := range []struct {
		in      string
		want    Value
		wantErr string
	}{
		{"", nil, `expected "foo"`},
		{"foo", "foo", ""},
		{"food", nil, `expected "foo"`},
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

func TestParse(t *testing.T) {
	for _, test := range []struct {
		name    string
		p       Parser
		in      string
		want    Value
		wantErr string // error string must contain this
	}{
		{
			name: "Empty",
			p:    Empty,
			in:   "",
			want: nil,
		},
		{
			name:    "unconsumed",
			p:       Empty,
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
			name: "Equal2",
			p:    And(Equal("foo"), Equal("d")),
			in:   "food",
			want: []Value{"foo", "d"},
		},
		{
			name: "Equal3",
			p:    And(Equal("foo"), Equal("d")),
			in:   "  foo\td\n",
			want: []Value{"foo", "d"},
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
			name: "While2",
			p:    And(While("letters", unicode.IsLetter), One("digit", unicode.IsDigit)),
			in:   "abc3",
			want: []Value{"abc", "3"},
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
			name: "Regexp2",
			p:    And(Regexp("abcs", `(abc)+`), Equal("abd")),
			in:   "abcabcabd",
			want: []Value{"abcabc", "abd"},
		},
		{
			name: "And",
			p:    And(Word("foo"), Word("bar")),
			in:   "foo bar",
			want: []Value{"foo", "bar"},
		},
		{
			name: "nested And",
			p:    And(Word("foo"), And(Word("bar"), Word("baz")), Word("boo")),
			in:   "foo bar baz boo",
			want: []Value{"foo", []Value{"bar", "baz"}, "boo"},
		},
		{
			name: "Do",
			p: Word("foo").Do(func(v Value) Value {
				return strings.ToUpper(v.(string))
			}),
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
		{
			name: "Or empty",
			p:    And(Or(Word("a"), Empty), Word("c")),
			in:   "c",
			want: []Value{"c"},
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
			want: []Value{"x"},
		},
		{
			name: "Repeat 2",
			p:    Repeat(Word("x")),
			in:   "x x",
			want: []Value{"x", "x"},
		},
		{
			name: "Repeat multi",
			p:    Repeat(And(One("letter", unicode.IsLetter), One("digit", unicode.IsDigit))),
			in:   "a1c3e 5 g 7",
			want: []Value{[]Value{"a", "1"}, []Value{"c", "3"}, []Value{"e", "5"}, []Value{"g", "7"}},
		},
		{
			name: "List",
			p:    List(Regexp("id", `[a-z]+`), Equal(",")),
			in:   "a, b, c, d",
			want: []Value{"a", "b", "c", "d"},
		},
		{
			name: "without Cut",
			p:    Or(And(Word("a"), Word("b")), Word("c")),
			in:   "a d",
			// We'd like "expected b",but we get this instead:
			wantErr: `parse failed`,
		},
		{
			name:    "Cut",
			p:       Or(And(Word("a"), Cut, Word("b")), Word("c")),
			in:      "a d",
			wantErr: `expected "b"`,
		},
		{
			name: "nested Cut",
			// Inner cut doesn't affect outer Or.
			p: Or(
				And(
					Or(And(Word("a"), Cut, Word("b")), Word("c")),
					Word("d")),
				Word("e")),
			in:      "f",
			wantErr: "parse failed",
		},
		{
			name: "not Skipping",
			p:    And(Word("notabs"), While("any", func(rune) bool { return true })),
			in:   "notabs  \t\tx",
			want: []Value{"notabs", "x"},
		},
		{
			name: "Skipping",
			p: And(
				Word("notabs"),
				Skipping(func(r rune) bool { return r == ' ' },
					While("any", func(rune) bool { return true }))),
			in:   "notabs  \t\tx",
			want: []Value{"notabs", "\t\tx"},
		},
		{
			name: "Skipping nil",
			p: And(
				Word("notabs"),
				Skipping(nil,
					While("any", func(rune) bool { return true }))),
			in:   "notabs  \t\tx",
			want: []Value{"notabs", "  \t\tx"},
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			got, err := test.p.Parse(test.in)
			if err != nil {
				if test.wantErr != "" {
					if !strings.Contains(err.Error(), test.wantErr) {
						t.Fatalf("got '%v', wanted error containing %q", err, test.wantErr)
					}
					return
				}
				t.Fatalf("got '%v', want success", err)
			}
			if test.wantErr != "" {
				t.Fatalf("got success, want error containing %q", test.wantErr)
			}
			if diff := cmp.Diff(test.want, got); diff != "" {
				t.Errorf("mismatch (-want, +got)\n%s", diff)
			}
		})
	}
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

func TestFlatten(t *testing.T) {
	for _, test := range []struct {
		in, want []Value
	}{
		{nil, nil},
		{[]Value{1, 2, 3}, []Value{1, 2, 3}},
		{[]Value{1, []Value{2, 3, 4}, 5}, []Value{1, 2, 3, 4, 5}},
		{[]Value{1, []Value{[]Value{2, 3}, 4, 5}, 6}, []Value{1, 2, 3, 4, 5, 6}},
	} {
		got := flatten(test.in)
		if !cmp.Equal(got, test.want) {
			t.Errorf("got %v, want %v", got, test.want)
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

	limitClause := And(Word("limit"), Cut, Int).Do(func(vs []Value) Value { return vs[1] })

	p := And(And(
		Word("select"),
		Or(Equal("*"), List(ident, Equal(","))).Do(
			func(v Value) Value {
				q := &query{}
				if _, ok := v.(string); !ok {
					for _, id := range v.([]Value) {
						q.selects = append(q.selects, id.(string))
					}
				}
				return q
			}),
		Word("from"),
		ident).Do(func(vs []Value) Value {
		q := vs[1].(*query)
		q.coll = vs[3].(string)
		return q
	}),
		Optional(limitClause)).Do(func(vs []Value) Value {
		q := vs[0].(*query)
		if len(vs) > 1 {
			q.limit = vs[1].(int64)
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

func TestConvertDoFunc(t *testing.T) {
	arg := []Value{1}
	for _, test := range []struct {
		f       interface{}
		wantVal Value
		wantErr error
	}{
		{func(v Value) (Value, error) { return 2, io.EOF }, 2, io.EOF},
		{func(v Value) Value { return 2 }, 2, nil},
		{func(v Value) {}, arg, nil},
		{func(vs []Value) (Value, error) { return 2, io.EOF }, 2, io.EOF},
		{func(vs []Value) Value { return 2 }, 2, nil},
		{func(vs []Value) {}, arg, nil},
	} {
		gf, err := convertDoFunc(test.f)
		if err != nil {
			t.Fatalf("%v: %v", reflect.TypeOf(test.f), err)
		}
		gotVal, gotErr := gf(arg)
		if !cmp.Equal(gotVal, test.wantVal) || gotErr != test.wantErr {
			t.Errorf("%v: got (%v, %v), want (%v, %v)",
				reflect.TypeOf(test.f), gotVal, gotErr, test.wantVal, test.wantErr)
		}
	}

	for _, f := range []interface{}{
		17,
		func(int, int) {},
		func(string) {},
		func(Value) int { return 0 },
		func(Value) (Value, int) { return nil, 0 },
		func(Value) (Value, int, error) { return nil, 0, nil },
	} {
		_, err := convertDoFunc(f)
		if err == nil {
			t.Errorf("%v: got success, want error", reflect.TypeOf(f))
		}
	}
}
