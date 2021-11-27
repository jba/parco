// Copyright 2021 Jonathan Amsterdam.

package parco

import (
	"strconv"
	"strings"
	"testing"
	"unicode"

	"github.com/google/go-cmp/cmp"
)

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
			want: nil,
		},
		{
			name: "Any",
			p:    Any,
			in:   "x",
			want: "x",
		},
		{
			name: "Lit",
			p:    Lit("foo"),
			in:   "foo",
			want: "foo",
		},
		{
			name:    "Lit fail",
			p:       Lit("foo"),
			in:      "bar",
			wantErr: `expected "foo"`,
		},
		{
			name:    "unconsumed",
			p:       Lit("foo"),
			in:      "foo bar",
			wantErr: `unconsumed input starting at "bar"`,
		},
		{
			name: "Is",
			p:    Is("ident", isIdent),
			in:   "alpha",
			want: "alpha",
		},
		{
			name:    "Is fail",
			p:       Is("ident", isIdent),
			in:      "123",
			wantErr: "expected ident",
		},
		{
			name: "And",
			p:    And(Lit("foo"), Lit("bar")),
			in:   "foo bar",
			want: []Value{"foo", "bar"},
		},
		{
			name: "nested And",
			p:    And(Lit("foo"), And(Lit("bar"), Lit("baz")), Lit("boo")),
			in:   "foo bar baz boo",
			want: []Value{"foo", []Value{"bar", "baz"}, "boo"},
		},
		{
			name: "Do",
			p:    Do(Lit("foo"), func(v Value) (Value, error) { return strings.ToUpper(v.(string)), nil }),
			in:   "foo",
			want: "FOO",
		},
		{
			name: "Or a",
			p:    Or(Lit("a"), Lit("b")),
			in:   "a",
			want: "a",
		},
		{
			name: "Or b",
			p:    Or(Lit("a"), Lit("b")),
			in:   "b",
			want: "b",
		},
		{
			name:    "Or fail",
			p:       Or(Lit("a"), Lit("b")),
			in:      "c",
			wantErr: `parse failed at "c"`,
		},
		{
			name: "Or empty",
			p:    And(Or(Lit("a"), Empty), Lit("c")),
			in:   "c",
			want: []Value{"c"},
		},
		{
			name: "Repeat empty",
			p:    Repeat(Lit("x")),
			in:   "",
			want: nil,
		},
		{
			name: "Repeat 1",
			p:    Repeat(Lit("x")),
			in:   "x",
			want: []Value{"x"},
		},
		{
			name: "Repeat 2",
			p:    Repeat(Lit("x")),
			in:   "x x",
			want: []Value{"x", "x"},
		},
		{
			name: "Repeat multi",
			p:    Repeat(And(Any, Any)),
			in:   "a b c d e f g h",
			want: []Value{[]Value{"a", "b"}, []Value{"c", "d"}, []Value{"e", "f"}, []Value{"g", "h"}},
		},
		{
			name: "List",
			p:    List(Is("id", isIdent), Lit(",")),
			in:   "a , b , c , d",
			want: []Value{"a", "b", "c", "d"},
		},
		{
			name: "without Cut",
			p:    Or(And(Lit("a"), Lit("b")), Lit("c")),
			in:   "a d",
			// We'd like "expected b",but we get this instead:
			wantErr: `parse failed at "a"`,
		},
		{
			name:    "Cut",
			p:       Or(And(Lit("a"), Cut, Lit("b")), Lit("c")),
			in:      "a d",
			wantErr: `expected "b"`,
		},
		{
			name: "nested Cut",
			// Inner cut doesn't affect outer Or.
			p: Or(
				And(
					Or(And(Lit("a"), Cut, Lit("b")), Lit("c")),
					Lit("d")),
				Lit("e")),
			in:      "f",
			wantErr: "parse failed",
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			got, err := Parse(test.p, strings.Fields(test.in))
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
		limit   int
	}

	Int := Do(Any, func(v Value) (Value, error) {
		return strconv.Atoi(v.(string))
	})

	limitClause := Do(
		And(Lit("limit"), Cut, Int),
		func(v Value) (Value, error) {
			return v.([]Value)[1], nil
		})

	p := Do(
		And(Do(
			And(
				Lit("select"),
				Do(
					Or(Lit("*"), List(Is("identifier", isIdent), Lit(","))),
					func(v Value) (Value, error) {
						q := &query{}
						if _, ok := v.(string); !ok {
							for _, id := range v.([]Value) {
								q.selects = append(q.selects, id.(string))
							}
						}
						return q, nil
					}),
				Lit("from"),
				Is("identifier", isIdent)),
			func(v Value) (Value, error) {
				vs := v.([]Value)
				q := vs[1].(*query)
				q.coll = vs[3].(string)
				return q, nil
			}),
			Opt(limitClause)),
		func(v Value) (Value, error) {
			vs := v.([]Value)
			q := vs[0].(*query)
			if len(vs) > 1 {
				q.limit = vs[1].(int)
			}
			return q, nil
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
			err: `expected "from", got "x"`,
		},
		{
			in:  "select a , from x",
			err: `expected "from", got "x"`,
		},
		{
			in:  "select * from cities and more",
			err: `unconsumed input starting at "and"`,
		},
		{
			in:  "query",
			err: `expected "select", got "query"`,
		},
		{
			in:  "select * from x limit b",
			err: `strconv.Atoi: parsing "b": invalid syntax`,
		},
	} {
		got, err := Parse(p, strings.Fields(test.in))
		if err == nil {
			if test.err != "" {
				t.Errorf("%q: got success, want error", test.in)
			} else if diff := cmp.Diff(&test.want, got, cmp.AllowUnexported(query{})); diff != "" {
				t.Errorf("%q: mismatch (-want, +got)\n%s", test.in, diff)
			}
		} else {
			if test.err == "" {
				t.Errorf("%q: got %v, want success", test.in, err)
			} else if g := err.Error(); g != test.err {
				t.Errorf("%q, error:\ngot:  %s\nwant: %s", test.in, g, test.err)
			}
		}
	}
}

func isIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if i == 0 && !(r == '_' || unicode.IsLetter(r)) {
			return false
		}
		if i > 0 && !(r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)) {
			return false
		}
	}
	return true
}
