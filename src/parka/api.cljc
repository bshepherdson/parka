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
  (:refer-clojure :exclude [+ * and compile do drop let not str])
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    [parka.errors :as errors]
    [parka.machine.compiler :as compiler]
    [parka.machine.peg :as engine])
  #?(:cljs (:require-macros [parka.api])))

;;;; Parsing expression builders
(defn alt
  "Attempts each expression in order. When one matches, the `alt` is done.
  If all expressions fail, so does the `alt`."
  [& exprs]
  {:parka/type :parka/alt
   :parka/alts exprs})

(defn action
  "Given an expression and a unary function, runs that action function over the
  parse result returned by the inner expression."
  [expr f]
  {:parka/type   :parka/action
   :parka/action f
   :parka/inner  expr})

(defn value
  "Pipelined version of [[action]], resembles [[->]].

  Given an expression and 0 or more unary functions, runs the functions left to
  right over the parse result returned by the inner expression."
  [expr & fs]
  (action expr (apply comp (reverse fs))))

(defn group
  "Given 1 or more expressions, parses them all and returns a vector of their
  results. This is the proper name of the `[...]` shorthand."
  [& exprs]
  {:parka/type :parka/seq
   :parka/seq  exprs})

(defn between
  "Parses `expr` between `lhs` and `rhs` (3-arity) or with `around` on each
  side (2-arity).

  Note that the inner `expr` comes first, which is intended to allow
  `(-> :expr (p/between :whitespace) (p/between \\( \\)))`"
  ([expr around]
   (between expr around around))
  ([expr lhs rhs]
   (value (group lhs expr rhs) second)))

(defn do
  "Given 1 or more expressions, parses them all in order and returns the value
  of the last one, like [[core/do]]."
  [& exprs]
  (value (apply group exprs) last))

(defn drop
  "Parses an expr but drops its result, returning nil instead."
  [expr]
  (action expr (constantly nil)))

(defn str
  "Given a parsing `expr` that returns a list of strings, this concatenates the
  strings together.
  For example `:digit (p/str (p/+ (p/one-of \"0123456789\")))` returns the
  digit converted to a string."
  [expr]
  (action expr string/join))

(defn *
  "Given an inner expression, returns an expression that attempts to match it 0
  or more times.

  Be careful: parsers that parse nothing and succeed can cause infinite loops."
  [expr]
  {:parka/type  :parka/star
   :parka/inner expr})

(defn +
  "Like `*` but it matches 1 or more times.
   Equivalent to `[expr (* expr)]`."
  [expr]
  (action [expr (* expr)]
          (fn [[x xs]]
            (into [x] xs))))

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

(defn ?
  "Given a parsing `expr`, matches 0 or 1 copies of it.

  Equivalent to `(alt [(and expr) expr] (not expr))`."
  [expr]
  (alt (action [(and expr) expr]
               second)
       (not expr)))

;; TODO Add range, eg. (range \0 \9), (range \a \z)

(defn expecting
  "Parses like `expr` but in case of failure, overrides the default description
  of what was expected with the `msg`."
  [expr msg]
  {:parka/type  :parka/label
   :parka/inner expr
   :parka/label msg})

#?(:clj
   (defmacro let
     "This works like [[core/let]], but right-hand sides of the bindings
     are parsing expressions, rather than Clojure values.

     The body is regular Clojure code return a **plain value** - this becomes
     the value of the parser.

     For example:
     ```
     (p/let [x p
     y q]
     (fancy-fn x y 2))
     ;; is equivalent to
     (p/action [p q]
     (fn [[x y]]
     (fancy-fn x y 2)))
     ```"
     [bindings & body]
     (when-not (vector? bindings)
       (throw (ex-info "Bindings to parka.api/let must be in a vector." {})))
     (when-not (even? (count bindings))
       (throw (ex-info "parka.api/let must have an even number of bindings." {})))
     (core/let [pairs (partition 2 bindings)
                syms  (map first pairs)
                exprs (map second pairs)]
       `(action ~(vec exprs)
                (fn [[~@syms]]
                  ~@body)))))

(defn grammar
  "Given a map `rules` of `:label` to expression, and the `start` label, builds
  a grammar composed of many named expressions.
  These expressions can be (mutually and directly) recursive, but they cannot
  be \"left recursive\": `{:a [:a \"foo\"]}` causes an infinite loop."
  [rules start]
  {:parka/type  :parka/grammar
   :parka/rules rules
   :parka/start start})

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
  "Given a parsing `engine`, `source` label (eg. the file name), and input
  `text`, attempts to parse the input.

  The `engine` must be one compiled by `compile`.

  Returns either `{:success \"string matched\"}` or `{:error ...}`."
  [engine source text]
  (core/let [{:keys [error caps] :as res} (engine/run engine source text)]
    (when (not= 1 (count caps))
      (throw (ex-info "bad capture!" res)))
    (cond
      (core/and
        error (keyword? error)) {:error error}
      error        {:error (update error :parka/loc errors/pretty-location)}
      :else        {:success (peek caps)})))


(comment
  (parse (compile (let [_     \(
                        inner any
                        _     \)]
                    (str inner)))
         "<test>"
         "(x)")
  *e
  )

