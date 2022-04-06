# Design Notes

## PEG Machine

Parka works by compiling the user's _parsing expressions_ into a program for an
abstract parsing machine, then executing that program against a given input.

This two-step process aims to remove as much logic from the main parsing engine
as possible, so that it can run swiftly and simply.

The compilation process is contained in `parka.machine.compiler` and the
execution process in `parka.machine.peg`.

## Capturing and AST Construction

Given a grammar like this one (from `expr_test.clj`, with actions removed):

```clojure
{:start    [:expr p/eof]
 :expr     (p/alt [:mult    "+" :expr]
                  :mult)
 :mult     (p/alt [:primary "*" :mult]
                  :primary)
 :primary  (p/alt [\( :expr \)]
                  :number)
 :number   [(p/? \-) :decimal]
 :decimal  (p/+ :digit)
 :digit    (p/one-of "0123456789")}
```

- `\(` and `"+"` literals return strings (even character literals).
    - Same for `one-of`.
- `alt` returns whatever the winning alternative matched.
- `[]` sequences return a vector of the inner results.
- `?` optionals return the value if it matched, or nil if it didn't.
- `*` and `+` return a vector of results (possibly empty for `*`)

### Implementation

Capturing is handled as a kind of stack machine. Each expression captures for
itself, leaving the value on a stack. Combined captures, such as for `seq`, add
extra code to merge the individual results into a list.

The stack machine is powered by the following instructions:

- `[:drop]` drops the top of the stack
- `[:push value]` pushes an arbitrary Clojure value
- `[:mark]` pushes the current position in the input stream.
- `[:capture]` pops the starting index off the stack, and pushes the string from
  start inclusive to the current position exclusive.
- `[:apply-capture-1 f]` applies a unary function to TOS `(x -- y)`
- `[:apply-capture-2 f]` applies a binary function to NOS and TOS `(x y -- z)`

Note that the capture stack is part of the `:choice` stack, so backtracking
already reverts the capture stack properly.

