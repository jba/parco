package parco_test

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/jba/parco"
)

// A subset of Ian Cottrell's usage language.
// https://go-review.googlesource.com/c/exp/+/362537

type (
	expression interface {
		isExpression()
	}

	value struct { // positional arg
		name string
	}

	lit struct {
		name string
	}

	flags struct {
		aliases []string
		param   *string
	}

	optional struct {
		expression expression
	}

	repeat struct {
		expression expression
	}

	choice []expression

	sequence []expression
)

func (*value) isExpression()    {}
func (*lit) isExpression()      {}
func (*flags) isExpression()    {}
func (*optional) isExpression() {}
func (*repeat) isExpression()   {}
func (choice) isExpression()    {}
func (sequence) isExpression()  {}

func usageParser() parco.Parser[expression] {
	Eq := parco.Equal

	var choose parco.Parser[expression]

	ident := `\pL[-._\pL\pN]*`
	name := parco.Regexp("name", ident)

	literal := parco.Do(name,
		func(s string) expression { return &lit{s} })

	named := parco.Do(
		parco.Regexp("named", "<"+ident+">"),
		func(s string) expression { return &value{s[1 : len(s)-1]} })

	// A flag is a comma-separated list of flag names optionally followed by '=' name.
	flagName := parco.Do(
		parco.Regexp("flag name", "-"+ident),
		func(s string) string { return s[1:] })

	flag := parco.And2(
		parco.List(flagName, Eq(",")),
		parco.Optional(
			parco.And2(Eq("="), name,
				func(_, n string) string { return n }),
		),
		func(aliases []string, param *string) expression {
			return &flags{aliases: aliases, param: param}
		})

	optional := parco.And3(Eq("["), parco.Ptr(&choose), Eq("]"),
		func(_ string, e expression, _ string) expression {
			return &optional{expression: e}
		})

	group := parco.And3(Eq("("), parco.Ptr(&choose), Eq(")"),
		func(_ string, e expression, _ string) expression {
			return e
		})

	atom := parco.Or(optional, group, flag, named, literal)

	// A repeat is an atom optionally followed by "...".
	repeat := parco.And2(atom, parco.Optional(Eq("...")),
		func(e expression, dots *string) expression {
			if dots != nil {
				return &repeat{e}
			}
			return e
		})

	sequence := parco.Do(parco.Repeat1(repeat),
		func(es []expression) expression {
			if len(es) == 1 {
				return es[0]
			}
			return sequence(es)
		})

	choose = parco.Do(
		parco.List(sequence, Eq("|")),
		func(es []expression) expression {
			if len(es) == 1 {
				return es[0]
			}
			return choice(es)
		})

	return choose
}

func TestUsageLang(t *testing.T) {
	p := usageParser()
	for _, test := range []struct {
		in   string
		want expression
	}{
		{"lit", &lit{"lit"}},
		{"<arg>", &value{"arg"}},
		{"(lit)", &lit{"lit"}},
		{"[<opt>]", &optional{&value{"opt"}}},
		{
			"-f",
			&flags{aliases: []string{"f"}},
		},
		{
			"-flag,-f",
			&flags{aliases: []string{"flag", "f"}},
		},
		{
			"-a,-b=p",
			&flags{[]string{"a", "b"}, func() *string { s := "p"; return &s }()},
		},
		{
			"-a , -b  =  p",
			&flags{[]string{"a", "b"}, ptr("p")},
		},
		{
			"<file> ...",
			&repeat{&value{"file"}},
		},
		{
			"[-x]...",
			&repeat{&optional{&flags{aliases: []string{"x"}}}},
		},
		{
			"eat <food>",
			sequence([]expression{&lit{"eat"}, &value{"food"}}),
		},
		{
			"eat | sleep ...",
			choice([]expression{&lit{"eat"}, &repeat{&lit{"sleep"}}}),
		},
		{
			"eat|sleep ...",
			choice([]expression{&lit{"eat"}, &repeat{&lit{"sleep"}}}),
		},
		{
			"eat [-f=true] (<food> | candy)   |    play <game> with <player>...",
			choice([]expression{
				sequence([]expression{
					&lit{"eat"},
					&optional{&flags{[]string{"f"}, ptr("true")}},
					choice([]expression{&value{"food"}, &lit{"candy"}}),
				}),
				sequence([]expression{
					&lit{"play"},
					&value{"game"},
					&lit{"with"},
					&repeat{&value{"player"}},
				}),
			}),
		},
	} {
		got, err := p.Parse(test.in)
		if err != nil {
			t.Fatalf("%q: %v", test.in, err)
		}
		opt := cmp.AllowUnexported(lit{}, value{}, optional{}, flags{}, repeat{})
		if diff := cmp.Diff(test.want, got, opt); diff != "" {
			t.Errorf("%q: mismatch (-want, +got):\n%s", test.in, diff)
		}
	}
}

func ptr[T any](v T) *T { return &v }
