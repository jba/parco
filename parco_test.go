// Copyright 2021 Jonathan Amsterdam.

package parco

import (
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
		wantErr bool
	}{
		{
			name: "Lit",
			p:    Lit("foo"),
			in:   "foo",
			want: "foo",
		},
		{
			name: "And",
			p:    And(Lit("foo"), Lit("bar")),
			in:   "foo bar",
			want: []Value{"foo", "bar"},
		},
		// {
		// 	name: "Do",
		// 	p:    Do(Lit("foo"), func(_ *State, v Value) Value {

	} {
		t.Run(test.name, func(t *testing.T) {
			got, err := Parse(test.p, strings.Fields(test.in))
			if err != nil {
				if test.wantErr {
					return
				}
				t.Fatalf("got %v, want success", err)
			}
			if test.wantErr {
				t.Fatal("got success, want error")
			}
			if diff := cmp.Diff(test.want, got); diff != "" {
				t.Errorf("mismatch (-want, +got)\n%s", diff)
			}
		})
	}
}

// TODO: test nested Or with commit.

// func TestParseQuery(t *testing.T) {
// 	type query struct {
// 		selects []string
// 		coll    string
// 		limit   int
// 	}

// 	p := And(
// 		Lit("select"),
// 		Do(
// 			Or(Lit("*"), List(Is("identifier", isIdent), Lit(","))),
// 			func(s *State) {
// 				toks := s.Tokens()
// 				if toks[0] != "*" {
// 					for i := 0; i < len(toks); i += 2 {
// 						sels := &s.Value.(*query).selects
// 						*sels = append(*sels, toks[i])
// 					}
// 				}
// 			}),
// 		Lit("from"),
// 		Do(Is("identifier", isIdent), func(s *State) { s.Value.(*query).coll = s.Token() }),
// 		Opt(And(
// 			Lit("limit"),
// 			Cut,
// 			Do(Any, func(s *State) {
// 				n, err := strconv.Atoi(s.Token())
// 				if err != nil {
// 					s.Fail(err)
// 				}
// 				s.Value.(*query).limit = n
// 			}))))
// 	for _, test := range []struct {
// 		in   string
// 		want query
// 		err  string
// 	}{
// 		{
// 			in:   "select * from cities",
// 			want: query{selects: nil, coll: "cities"},
// 		},
// 		{
// 			in:   "select a , b , c from d",
// 			want: query{selects: []string{"a", "b", "c"}, coll: "d"},
// 		},
// 		{
// 			in:   "select * from cities limit 5",
// 			want: query{selects: nil, coll: "cities", limit: 5},
// 		},
// 		{
// 			in:  "select from x",
// 			err: `expected "from", got "x"`,
// 		},
// 		{
// 			in:  "select a , from x",
// 			err: `expected "from", got "x"`,
// 		},
// 		{
// 			in:  "select * from cities and more",
// 			err: `unconsumed input starting at "and"`,
// 		},
// 		{
// 			in:  "query",
// 			err: `expected "select", got "query"`,
// 		},
// 		{
// 			in:  "select * from x limit b",
// 			err: `strconv.Atoi: parsing "b": invalid syntax`,
// 		},
// 	} {
// 		got := &query{}
// 		err := Parse(p, strings.Fields(test.in), got)
// 		if err == nil {
// 			if test.err != "" {
// 				t.Errorf("%q: got success, want error", test.in)
// 			} else if diff := cmp.Diff(&test.want, got, cmp.AllowUnexported(query{})); diff != "" {
// 				t.Errorf("%q: mismatch (-want, +got)\n%s", test.in, diff)
// 			}
// 		} else {
// 			if test.err == "" {
// 				t.Errorf("%q: got %v, want success", test.in, err)
// 			} else if g := err.Error(); g != test.err {
// 				t.Errorf("%q, error:\ngot:  %s\nwant: %s", test.in, g, test.err)
// 			}
// 		}
// 	}
// }

// func TestRepeat(t *testing.T) {
// 	p := Repeat(Lit("x"))
// 	var xs []string
// 	for i := 0; i < 3; i++ {
// 		if err := Parse(p, xs, nil); err != nil {
// 			t.Fatal(err)
// 		}
// 		xs = append(xs, "x")
// 	}
// }

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