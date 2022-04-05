# Design Notes

## Capturing and AST Construction

Given a grammar like this one (from `expr_test.clj`):

```clojure
{:start    [:expr p/eof]
 :expr     (p/alt [:mult    "+" :expr] :mult)
 :mult     (p/alt [:primary "*" :mult] :primary)
 :primary  (p/alt [\( :expr \)] (p/+ :decimal))
 :decimal  (p/one-of "0123456789")}
```

Here are the types of AST construction we care about:

- Terminals and pseudo-terminals as strings.
    - `\c` and `#{\a \b \c}` return single-character strings
    - `"abc"` returns the matched string.
    - `:keywords-with-trailing-hyphen-` are concatenated to strings.
- `alt`s just return the capture of the winner.
    - Maybe it would be useful to label those?
- `seq`s return a map like this, for the first case of `:expr`: `{:mult ...
  :expr ...}`, that is it captures the nonterminals by name.
    - If there are multiple nonterminals with the same key, they are collected
      into a vector in order.
    - String-returning (pseudo-)terminals are dropped.
        - TODO Probably fix this with `{:label "terminal"}` pairs.
- `action` calls override the default with a function. They get passed the
  raw value from below, and whatever it returns becomes the value.

### Implementation

Capturing is handled as a kind of stack machine. Each expression captures for
itself, leaving the value on a stack. Combined captures, such as for `seq`, use
functions to combine the top few entries on the stack.

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

