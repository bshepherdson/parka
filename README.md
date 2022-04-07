# Parka - Parser Combinators in Clojure

Parka is a [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar)
parsing system that aims to make parsing both *simple* and *easy*.

It is respectable fast, but if you're looking to parse multi-megabyte files this
probably isn't the first place you'll want to look. See Performance below for
details.

## Usage Guide

Parsing works in two phases: compiling and parsing. You write a parsing
expression and compile it once, than run it on many input strings. Expressions
are just data structures, but you'll generally use the `parka.api` functions to
help create them.

Note that a *grammar* is just one type of parsing expression. A grammar is used
to give names to some subexpressions, and then parse from a starting
subexpression.

An example will go a long way:

```clojure
(ns my.parser
  (:require
    [parka.api :as p]))

(def rules
  {:number     (p/+ (p/one-of "0123456789"))
   :subexpr    (p/alt ["(" :expr ")"]
                      :number)
   :expr2      [:subexpr (p/* [:mul-op :subexpr])]
   :expr       [:expr2   (p/* [:add-op :expr2])]
   :add-op     (p/alt "+" "-")
   :mul-op     (p/one-of "*/")
   :start      [:expr p/eof]})

(def parser (p/compile (p/grammar rules :start)))

(p/parse parser "myfile.txt" "12+6*9")
```

- `rules` is a map of keyword names to parsing expressions, which forms our
  grammar.
- A `:number` is one of more digit characters.
    - `(p/+ q)` parses 1 or more repetitions of `q`, returning a list.
    - So for example `:number` on `"123"` would parse as `["1" "2" "3"]`.
      - See below for a way to improve that output.
- `:subexpr` is a parenthesized `:expr` or a `:number`.
  - `p/alt` takes several expressions and tries them **in order**; the first to
    match is the value returned. `p/alt` fails if no alternatives match.
- Note the special forms: vectors, strings and keywords. See below on
  **Special Forms** for details.
- `:expr2` is a `:subexpr` followed by 0 or more `[:mul-op :subexpr]` pairs.
  - Eg. `"3*4/2"` parses as an `:expr2` with `[["3"] [["*" ["4"]] ["/" ["2"]]]]`.
- `:expr` is like `:expr2`, for `:add-op`s.
- `:add-op` use `p/alt` and `:mul-op` `p/one-of`, but the result is the same:
  matching one character operators.

Note that there's no code generation, macros, or other magic here - the
combinators are simply functions that return data structures.

### Special Forms


Several of the parsing functions are very commonly used, so a shorthand can
be used instead, to make parsers easier to read and write.

- `[p1 p2 p3]` is a sequence, which must all match in order.
  - The returned value is a vector of all the inner values.
- `:some-keyword` looks up that key in the grammar.
- `"literal string"` matches exactly that string, and returns that string.
- `\c` matches exactly a single character, returning the **string** `"c"`.
- `#{\s \f \d}` matches one character in the set. Returns the **string**.


### Whitespace

There's no special handling for whitespace - parse it and ignore it. Using
`p/pick` is handy here - there's often only one real value surrounded by
punctuation and whitespace. We might make the above `:subexpr` more flexible
with:

```clojure
:ws      (p/* (p/one-of " \t\r\n"))
:subexpr (p/pick [2] ["(" :ws :expr :ws ")"])
```

which parses the parens and optional whitespace, but returns just the inner
`:expr`.


### Actions

Very often, the raw values as returned by the parser are not the values we want
in our AST. For example, we want numeric literals as numbers, not as strings,
and we want to drop empty characters like whitespace.

This is achieved in a composable way by wrapping an inner parser with
`p/action`. This runs the inner parser, then feeds its results through a
function you provide, which becomes the value of the `p/action`.

So we might refine the `:number` literal parser above to be:

```clojure
:number (p/action
          (p/+ (p/one-of "0123456789"))
          (fn [chars]
            (Integer/parseInt (apply str chars))))
```

The second argument gives some information about the parsing context, such as
the location in the file for error messages. See the docs for `p/action` for
details.

Also `p/pick` and `p/str` are useful shorthands for certain kinds of actions:
`(p/pick path expr)` is equivalent to `(p/action expr #(get-in % path))`.
`p/str` concatenates a list of strings into one string.


### Example: JSON Parser

There's a complete JSON parser in
[`json_test.clj`](/shepheb/parka/tree/main/test/parka/json_test.clj) that should
serve as a nice example.

## Performance

As noted above, Parka is not aiming for speed on large files.

For reference, it parses the [8MB JSON.parse test file from
V8](https://github.com/GoogleChromeLabs/json-parse-benchmark) in 125s on my dev
machine. For comparison, V8's JavaScript parser on the same machine takes
18-20s and JSON.parse takes 12-13s, so Parka is roughly 10x slower.

In practice, it's adequate for parsing programs and configuration files, but not
for bulky log files or outputs.

