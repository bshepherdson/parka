(ns parka.api
  "Main namespace for building and using Parka's PEG parsers.
  The functions in this file create Parka structures to describe, for example,
  a choice between several expressions.
  Such an expression can be compiled into a parser engine, and then this engine
  can be executed repeatedly on input strings.

  Parser expressions are constructed from the following:
  - Characters parse exactly themselves.
  - Strings parse as a sequence of characters, exactly as given.
  - Sets of Characters parse any one character which is a member of the set.
  - A vector is a nestable sequence (equivalent to calling `seq`).
  - Keywords reference a nonterminal in the same grammar.
  - Each of the parser combinators in this namespace produces an expression.

  A grammar is a map from keyword labels to parsing expressions, plus a start
  symbol. Grammars are themselves a parsing expression and can be nested.

  Once you have constructured your complete expression, call `compile` with it.
  The returned engine can be run by calling `parse`."
  (:refer-clojure :exclude [and compile not seq])
  (:require
    [parka.machine.compiler :as compiler]
    [parka.machine.peg :as engine]))

;;;; Parsing expression builders
(defn seq
  "Runs a sequence of parsers in order, failing if any of them fail.
  Passing a vector of parsers is an equivalent shorthand."
  [& exprs]
  (vec exprs))

(defn alt
  "Attempts each expression in order. When one matches, the `alt` is done.
  If all expressions fail, so does the `alt`."
  [& exprs]
  {:parka/type :parka/alt
   :parka/alts exprs})

(defn many
  "Given an inner expression, returns an expression that attempts to match it 0
  or more times.

  Be careful: parsers that parse nothing and succeed can cause infinite loops."
  [expr]
  {:parka/type  :parka/star
   :parka/inner expr})

(defn one-of
  [chs]
  (into #{} chs))

(defn and
  "Positive look-ahead. Attempts to parse `expr`, failing if `expr` fails.
  Either way, `(and expr)` consumes no input."
  [expr]
  {:parka/type  :parka/and
   :parka/inner expr})

(defn not
  "Negative look-ahead. Attempts to parse `expr`, failing if `expr` passes and
  passing if `expr` fails.
  Either way, `(not expr)` consumes no input."
  [expr]
  {:parka/type  :parka/not
   :parka/inner expr})

(defn opt
  "Given a parsing `expr`, matches 0 or 1 copies of it.

  Equivalent to `(alt [(and expr) expr] (not expr))`."
  [expr]
  (alt [(and expr) expr] (not expr)))

(def any
  "Matches any single character. Not a function, just a constant."
  {:parka/type :parka/any})

(def eof
  "Matches end-of-file. Useful for ensuring the entire input is consumed."
  (not any))

;;;; Top-level functions
(defn compile [expr]
  (compiler/compile-expr expr))

(defn parse
  "Given a parsing `engine`, source `label` (eg. the file name), and input
  `text`, attempts to parse the input.

  The `engine` must be one compiled by `compile`.

  Returns either `{:success \"string matched\"}` or `{:error ...}`."
  [engine label text]
  (let [{:keys [error input pos]} (engine/run engine label text)]
    (if error
      {:error error}
      {:success (subs input 0 pos)})))


(comment
  (parse (compile [\a \b \c
                   {:parka/type :parka/not
                    :parka/inner {:parka/type :parka/any}}])
         "<test>"
         "abc")
  )

; p? is &pp / !p
; p+ is pp*

