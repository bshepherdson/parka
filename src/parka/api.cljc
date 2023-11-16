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
  (:refer-clojure :exclude [+ * and compile do drop let not range str])
  (:require
    [clojure.core :as core]
    [clojure.string :as string]
    [parka.dynamic :as dynamic]
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

#_(defn alt-labels
  "Like [[alt]], but the args are alternating human-readable labels and parsing
  expressions. `(alt-labels \"abc\" :abc \"def\" :def)` is a shorthand for
  `(alt (expecting :abc \"abc\") (expecting :def \"def\"))`."
  [& args]
  (when-not (even? (count args))
    (-> "alt-labels expects (alt-labels label1 expr1 ...), but got %d args"
        (format (count args))
        (ex-info {:args args})
        throw))
  (apply alt (for [[label expr] (partition 2 args)]
               (expecting expr label))))

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

(defn range
  "Parses any single character whose Unicode codepoint falls between `start` and
  `end` inclusive."
  [start end]
  (core/let [exp   (core/str "[" start "-" end "]")
             start (int start)
             end   (inc (int end))
             inner {:parka/type :parka/set
                    :parka/set  (set (map char (core/range start end)))}]
    (expecting inner exp)))

(defn match
  "Matches a single character that returns a truthy value for `pred`."
  [pred]
  {:parka/type :parka/set
   :parka/set  pred})

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

(defn until
  "Matches 0 or more `expr`, until `end` matches. Consumes `end`!"
  [expr end]
  (let [parts (* (do (not end) expr))
        _ end]
    parts))

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
  (expecting (not any) "EOF"))

(defn fail
  "Returns a parser that always fails, with the given error message."
  [msg]
  (expecting (not any) msg))

;;;; Top-level functions
(defn compile [expr]
  (compiler/compile-expr expr))

(defn parse
  "Given a parsing `engine`, `source` label (eg. the file name), and input
  `text`, attempts to parse the input.

  The `engine` must be one compiled by `compile`.

  Returns either `{:success \"string matched\"}` or `{:error ...}`.

  The input must be fully consumed, or an error is returned."
  [engine source text]
  (core/let [{:keys [error caps pos] :as res} (engine/run engine source text)]
    (when (core/and (core/not error)
                    (not= 1 (count caps)))
      (throw (ex-info "bad capture!" res)))
    (cond
      (core/and error (keyword? error))
      {:error error}

      error
      {:error (update error :parka/loc errors/pretty-location)}

      #_#_(< pos (count text))
      {:error {:parka/parse-error true
               :parka/loc         (errors/pretty-location [source text pos])
               :parka/tail        (subs text pos (min (count text)
                                                      (core/+ pos 30)))}}

      :else {:success (peek caps)})))

(defn parse-dynamic
  "Given a parsing expression `expr`, `source` label (eg. a file name), and
  input `text`, attempts to parse the input.

  Returns either `{:success value :tail remaining-input-str}`
  or `{:error #:parka{:parse-error true :loc location :tail input-tail}}`."
  [expr source text]
  (core/let [[res s value :as result] (dynamic/parse-string expr source text)]
    (case res
      :success (merge {:success value}
                      (when (< (:pos s) (count text))
                        {:tail (subs text (:pos s))}))
      :fail    (core/let [details @s]
                 ;; TODO: Return a snippet around the error (to each side)
                 {:error {:parka/parse-error  true
                          :parka/loc (errors/pretty-location
                                       [source text (:pos details)])
                          :parka/expectations (dynamic/expectations details)}})
      :error   (throw (ex-info (str "Bad input parser: " #_really-err-msg s)
                               {:error s})))))
