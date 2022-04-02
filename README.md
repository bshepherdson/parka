# Parka - Parser Combinators in Clojure

Parka is a [parser combinator](https://en.wikipedia.org/wiki/Parser_combinator)
library that aims to be both *simple* and *easy*.

That comes at a cost, mostly a performance cost. If you're looking to parse
large files, you're in the wrong place.

## Usage Guide

Parsing works in two phases: constructing and parsing. The combinators are
functions that return parsers (which are just data structures).

At the top level a *grammar* is a map of keywords to parsers, plus the starting
keyword. These are comparable to the *production rules* of an EBNF grammar.

Parsing a complete input begins from a start parser, by default called `:start`.

An example will go a long way:

```clojure
(ns my.parser
  (:require
    [parka.core :as p]))

(def rules
  {:number     (p/many1 (p/span \0 \9))
   :subexpr    (p/alt (p/between "(" ")" :expr)
                      :number)
   :expr2      (p/pseq :subexpr (p/many (p/pseq :mul-op :subexpr)))
   :expr       (p/pseq :expr2   (p/many (p/pseq :add-op :expr2)))
   :add-op     (p/alt "+" "-")
   :mul-op     (p/one-of "*/")})

(def grammar (p/grammar rules :expr))

(p/parse-str grammar "myfile.txt" "12+6*9")
```

- As noted above, a grammar is simply a map of rule names to parsers.
- A `:number` is a list of digit characters.
    - `(p/span lo hi)` matches any single character between `lo` and `hi`
      inclusive.
    - `(p/many1 q)` parses 1 or more repetitions of `q`, returning a list.
    - So for example `"123"` would parse as `[\1 \2 \3]`.
    - See below for a way to improve that output.
- `:subexpr` is a parenthesized `:expr` or a `:number`.
- Note the two special cases:
  - A bare string is equivalent to `(p/lit "...")`.
  - A bare keyword is equivalent to `(p/sym :keyword)`, referencing a rule of
    the grammar.
- `:expr2` is a `:subexpr` followed by 0 or more `[:mul-op :subexpr]` pairs.
  - Eg. `"3*4/2"` parses as an `:expr2` with `[[\3] [[\* \4] [\/ \2]]]`.
- `:expr` is like `:expr2`, for `:add-op`s.
- `:add-op` use `p/alt` and `:mul-op` `p/one-of`, but the result is the same:
  matching one character operators.

Note that there's no code generation, macros, or other magic here - the
combinators are simply functions that return data structures.

### Shorthands

Several of the parsing functions are very commonly used, so a shorthand can
be used instead, to make parsers easier to read and write.

- `[p1 p2 p3]` is equivalent to `(pseq p1 p2 p3)`.
- `:some-keyword` is equivalent to `(sym :some-keyword)`, looking up that parser
  by name in the grammar.
- `"literal string"` is equivalent to `(lit "literal-string")`.


### Whitespace

There's no special handling for whitespace - parse it and discard it. Using
`p/pseq-at` is handy here - there's often only one real value surrounded by
punctuation and whitespace. We might make the above `:subexpr` more flexible
with

```clojure
  :ws      (p/many (p/one-of " \t\r\n"))
  :subexpr (p/pseq-at 2 "(" :ws :expr :ws ")")
```

which parses the parens and optional whitespace, but returns just the inner
`:expr`.


### Actions

Very often, the raw values as returned by the parser are not the values we want
in our AST. For example, we want numeric literals as numbers, not as strings or
lists of characters.

This is achieved in a composable way by wrapping an inner parser with
`p/with-action`. This runs the inner parser, then feeds its results through a
function you provide, which becomes the value of the `p/with-action`.

So we might refine the `:number` literal parser above to be:

```clojure
  :number (p/with-action
            (p/many1 (p/span \0 \9))
            (fn [chars _]
              (Integer/parseInt (apply str chars))))
```

The second argument gives some information about the parsing context, such as
the location in the file for error messages. See the docs for `p/with-action`
for details.


### Example: JSON Parser

There's a complete JSON parser in
[`json_test.clj`](/shepheb/parka/tree/main/test/parka/json_test.clj) that should
serve as a nice example.

