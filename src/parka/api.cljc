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
  (:refer-clojure :exclude [compile])
  (:require
    [parka.machine.compiler :as compiler]
    [parka.machine.peg :as engine]))

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

