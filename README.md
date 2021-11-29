# parco

A combinator-based parser in Go.

generic version:

- "And" can't elide nils anymore.

- And2 really helped...tuples?

- Can't just write `Cut`, must write `Cut[string]()`.

- Table driven test much clumsier, but do-able.
- One idea:
```
    for _, test := range []interface{ run(*testing.T) }{
        test[string]{
            name: "Empty",
            p:    Empty[string](),
            in:   "",
            want: "",
        },
        test[string]{
            name:    "unconsumed",
            p:       Empty[string](),
            in:      "x",
            wantErr: "unconsumed input",
        },
        test[string]{
            name: "Equal",
            p:    Equal("foo"),
            in:   "foo",
            want: "foo",
        },
        test[[]string]{ ...}
```
Ended up with multiple loops to take advantage of nested literal shortcut.
