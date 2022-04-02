(ns parka.core
  "Core functionality for the Parka parser combinator library.

  Most of the functions in this namespace consume literals and parsers and
  return parsers.

  The core parsing function is a multimethod,
  ```clojure
  (parse parser stream state) ; returns an updated stream
  ```

  It either consumes some input text and produces the new stream, or throws a
  parse error.

  Note that `(parse :some-keyword ...)` is treated as `(sym :some-keyword)`,
  and `(parse \"a string\" ...)` is treated as `(lit \"a string\")`."
  (:refer-clojure :rename {not core-not})
  (:require
    [clojure.string :as string]
    [parka.errors :as errs]
    [parka.streams :as st]))

(defn location [s]
  (select-keys s [:filename :line :col]))

(defmulti parse (fn [p _]
                  (cond
                    (map? p)     (:type p)
                    (keyword? p) :sym
                    (string? p)  :lit)))

(defn try-parse
  "Wraps a `(parse p s)` call, catching any parse error.
  Returns `[success? result]` instead."
  [p s]
  (try
    [true (parse p s)]
    (catch #?(:clj Exception :cljs js/Error) e
      [false e])))


; lit parses a given string exactly, matching case.
(defn lit
  "Returns a parser that matches the given `target` string exactly,
  case-sensitive.
  The parsed value is the string."
  [target]
  {:type   :lit
   :target target})

(defmethod parse :lit [p s]
  (let [target (or (:target p) p) ; Auto-upgrade strings to literals.
        desc   (str "literal '" target "'")]
    (st/set-value (reduce (fn [s ch] (st/expect-char s #(= ch %) desc))
                          s (seq target))
                  target)))

;; Lit IC
(defn- upcase [ch]
  (-> ch str string/upper-case first))

(defn lit-ic
  "Returns a parser that matches the `target` string, ignoring case.
  The parsed value is the given string - that is, the string you provide to
  `lit-ic`, not whatever case the user typed."
  [target]
  {:type    :lit-ic
   :target  target
   :upcased (string/upper-case target)})

(defmethod parse :lit-ic [{:keys [target upcased]} s]
  (let [desc (str "literal '" target "'")]
    (st/set-value (reduce (fn [s ch] (st/expect-char s #(= ch (upcase %)) desc))
                       s (seq upcased))
               target)))


;; Alt
(defn alt
  "Returns a parser that tries each of several alternatives in a row, returning
  the first to succeed.
  If they all fail, returns a combined error that describes the union of all
  legal next tokens."
  [& alts]
  {:type :alt
   :alts alts})

(defmethod parse :alt [{:keys [alts]} s]
  (loop [ps alts
         errs []]
    (if (empty? ps)
      ; Failed all parsers; return the concatenated error.
      (let [exps (mapcat (comp :parka/expectations ex-data) errs)]
        (errs/failed-expect s exps))
      ; Still some, so try the first one.
      (let [[pass? result] (try-parse (first ps) s)]
        (if pass?
          result
          (recur (rest ps) (conj errs result)))))))


(defn pseq
  "Returns a parser that runs a list of parsers in order, one after the other.
  If each succeeds, returns a seq of their values.
  If any child parser fails, so does `pseq` (consuming no input)."
  [& ps]
  {:type    :seq
   :parsers ps})

(defmethod parse :seq [{:keys [parsers]} s]
  (let [results (reductions #(parse %2 %1) s parsers)]
    ; We want the text position of the last one, but with the value being a
    ; vector of all of them together.
    (st/set-value (last results)
                  (mapv :value (rest results)))))

(defn pseq-at
  "A variant of [[pseq]] that takes as its first arg an `index` (0-based).
  Parses the same way as [[pseq]], but the parsed value is the `index`th
  element only, rather than the whole list."
  [index & ps]
  {:type  :seq-at
   :inner (apply pseq ps)
   :index index})

(defmethod parse :seq-at [{:keys [inner index]} s]
  (let [result (parse inner s)]
    (update result :value nth index)))


;; With-action and stringify.
(defmethod parse :action [{:keys [action inner]} s]
  (update (parse inner s) :value action
          {:loc   (location s)
           :state (-> s :state :user)}))

(defn with-action
  "Wraps a parser `p` and a function `f` into a new parser.
  Runs `p`, then passes its value and the context to the function:
  ```clojure
  (f value {:loc loc :state state})
  ```
  where the `state` is the user `state` (optionally) passed to [[parse-str]]."
  [p f]
  {:type   :action
   :action f
   :inner  p})

(defn stringify
  "Wraps a parser `p`, and stringifies its list-shaped result with [[str]]."
  [p]
  (with-action p (fn [vs _] (apply str vs))))


(defn optional
  "Wraps the provided parser `p`, and attempts to run it.
  If it succeeds, that value is returned.
  If it fails, `nil` is returned without consuming any input."
  [p]
  {:type   :optional
   :inner  p})

(defmethod parse :optional [{:keys [inner]} s]
  (let [[matched? s']  (try-parse inner s)]
    (if matched?
      s'
      (assoc s :value nil))))

(def any
  "Parser that matches any single character, and fails at EOF.
  (This is a constant, not a function, because it's immutable.)"
  {:type :any-char})

(defmethod parse :any-char [_ s]
  (let [[ch eof?] (st/head s)]
    (if eof?
      (errs/parse-error s "unexpected EOF")
      (assoc (st/tail s) :value ch))))


(defn one-of
  "Returns a parser that matches any single character from a sequence of
  possibilities.
  The parsed value is that single Character.
  Called like `(one-of \"abcdef\")`, passing a single `seq`-able value."
  [chars]
  {:type     :one-of
   :original chars
   :options  (into #{} chars)})

(defmethod parse :one-of [{:keys [options original]} s]
  (let [[ch eof?] (st/head s)]
    (cond
      eof?         (errs/failed-expect-msg s "unexpected EOF" options)
      (options ch) (assoc (st/tail s) :value ch)
      :else        (errs/parse-error s (str "expected one of: " original)))))


(defn none-of
  "Returns a parser that matches any single character NOT in the given sequence.
  The parsed value is the single Character matched.
  Fails at EOF - parsing something is required."
  [chars]
  {:type     :none-of
   :original chars
   :options  (into #{} chars)})

(defmethod parse :none-of [{:keys [options original]} s]
  (let [[ch eof?] (st/head s)]
    (cond
      eof?         (errs/parse-error s "unexpected EOF")
      (options ch) (errs/parse-error s (str "found illegal " ch
                                            ", expected character other than "
                                            original))
      :else        (assoc (st/tail s) :value ch))))

(defn span
  "Given two characters, returns a parser for any single character in that
  range, inclusive.

  ```clojure
  (span \\a \\z)
  ```
  Parses any lowercase letter.
  Value is the parsed Character. Fails on EOF."
  [lo hi]
  {:type :span
   :lo   lo
   :hi   hi})

(defmethod parse :span [{:keys [lo hi]} s]
  (let [[ch eof?] (st/head s)]
    (cond
      eof?    (errs/parse-error s "unexpected EOF")
      (<= (int lo) (int ch) (int hi))   (assoc (st/tail s) :value ch)
      :else   (errs/parse-error s (str "expected character in range "
                                       lo " to " hi "; found " ch)))))


(defn many
  "Returns a parser that matches 0 or more copies of `p`.
  The parsed value is a sequence of the inner results."
  [p]
  {:type    :many-min
   :min     0
   :inner   p
   :capture true})

(defn many1
  "Returns a parser that matches 1 or more copies of `p`.
  The parsed value is a sequence of the inner results."
  [p]
  {:type    :many-min
   :min     1
   :inner   p
   :capture true})

(defn many-min
  "Returns a parser that matches `n` or more copies of `p`.
  The parsed value is a sequence of the inner results."
  [n p]
  {:type    :many-min
   :min     n
   :inner   p
   :capture true})

(defn many-drop
  "Parses 0 or more copies of `p`, like [[many]].
  Discards the results, so the parsed value is `nil`."
  [p]
  {:type  :many-min
   :min   0
   :inner p})


; The combined parser for the different values of many.
(defmethod parse :many-min [{:keys [min inner capture]} s]
  (let [[s2 found results e]
        (loop [s2      s
               found   0
               results (when capture (transient []))]
          (let [[success? res] (try
                             [true  (parse inner s2)]
                             (catch #?(:clj Exception :cljs js/Error) e
                               [false e]))]
            (if success?
              (recur res (inc found)
                     (when capture (conj! results (:value res))))
              [s2 found results res])))]
    ; Check we got at least min results.
    (cond
      (< found min) (errs/failed-expect-msg s2 (str "minimum " min)
                                          (errs/expectations e))
      capture       (assoc s2 :value (persistent! results))
      :else         (assoc s2 :value nil))))


(defn sep-by
  "Given two parsers, `p` and `sep`, returns a new parser that matches 0 or
  more copies of `p`, separated by `sep`.
  The parsed value is a list of `p`'s results.
  Does NOT consume a trailing separator."
  [p sep]
  {:type  :sep-by
   :first p
   :tail  (many (pseq-at 1 sep p))
   :min   0})

(defn sep-by1
  "Variant of [[sep-by]] that requires at least 1 value."
  [p sep]
  {:type  :sep-by
   :first p
   :tail  (many (pseq-at 1 sep p))
   :min   1})

(defmethod parse :sep-by [{:keys [first tail min]} s]
  (let [[pass?      result] (try-parse first s)
        [tail-pass? tails]  (if pass?
                              (try-parse tail result)
                              [false nil])
        results             (cond
                              tail-pass? (update tails :value
                                                 #(cons (:value result) %))
                              pass?      (update result :value vector)
                              :else      (assoc s :value []))]
    (if (>= (count (:value results)) min)
      results
      (errs/parse-error s (str "expected at least " min ": " result)))))

(defn end-by
  "Variant of [[sep-by]] that parser 0 or more copies of `p`, separated
  **and followed** by `sep`.
  The value is a list of `p`'s results."
  [p sep]
  (many (pseq-at 0 p sep)))

(defn end-by1
  "Variant of [[end-by]] that requires at least 1 match."
  [p sep]
  (many1 (pseq-at 0 p sep)))


(defn many-till
  "Finds 0 or more instances of `p`, until `terminator` matches.
  This is 'non-greedy' in the regular expression sense: it tries to parse
  `terminator` repeatedly, and as soon as it succeeds `many-till` returns.

  Only when parsing the `terminator` fails does it try to run `p`.

  The output value is a list of `p`'s results, possibly empty.
  If both `terminator` and `p` fail at the same point, `many-till` fails.

  Some examples:
  ```clojure
  ; double-quoted string
  (pseq-at 1 (lit \"\\\") (many-till any (lit \"\\\")))
  ; Java-style line comment
  (pseq (lit \"//\") (many-till any (lit \"\\n\")))
  ```"
  [p terminator]
  {:type :many-till
   :terminator terminator
   :inner p})

(defmethod parse :many-till [{:keys [inner terminator]} s]
  (loop [s2      s
         results (transient [])]
    (let [[term? s3] (try-parse terminator s2)]
      (if term?
        (assoc s3 :value (persistent! results))
        (let [[inner? s4] (try-parse inner s2)]
          (if inner?
            (recur s4 (conj! results (:value s4)))
            (errs/failed-expect s2 (concat (errs/expectations s3)
                                           (errs/expectations s4)))))))))


(defn between
  "For wrapping opening and closing parsers around an inner parser, eg.
  parentheses and quotes.
  With three arguments, `[open close inner]`;
  with two arguments, `[open-and-close inner]`.
  In both cases, returns only the `inner` parser's value."
  ([ends inner] (between ends ends inner))
  ([start end inner]
   (pseq-at 1 start inner end)))

(defn lookahead
  "Wraps an inner parser. Fails if it does. If the inner parser succeeds, this
  succeeds too but consumes no input."
  [inner]
  {:type  :lookahead
   :inner inner})

(defmethod parse :lookahead [p s]
  (parse (:inner p) s)
  (dissoc s :value))

(defn not
  "Negative lookahead. Takes a parser. If that parser passes, not throws an
  error; if it fails not succeeds without consuming any input."
  [inner]
  {:type  :not
   :inner inner})

(defmethod parse :not [p s]
  (let [matched? (try
                   (parse (:inner p) s)
                   true
                   (catch Exception _
                     false))]
    (if matched?
      (throw (errs/parse-error s "not clause parsed successfully"))
      (dissoc s :value))))

(defn sym
  "References a named parser in the symbol table, generally by a keyword.
  Note that you rarely actually need to call this, since these are equivalent:
  ```clojure
  (parse :some-keyword ....)
  (parse (sym :some-keyword) ...)
  ```"
  [name]
  {:type :sym
   :name name})

(defmethod parse :sym [p s]
  (let [name  (or (:name p) p) ; Special case: auto-upgrading keywords to syms
        inner (get-in s [:state :symbols name])]
    (if inner
      (parse inner s)
      (errs/parse-error s (str "Unrecognized parser name: " name)))))


(defmethod parse :grammar [{:keys [symbols start]} s]
  ; On the way in, attach the :symbols to the stream's state.
  ; On the way out, restore the previous :symbols. This allows nesting of
  ; grammars, though that's an unusual thing to do.
  (let [inner    (get symbols start)]
    (when (core-not inner)
      (errs/parse-error s (str "start symbol " start " is not defined")))
    (assoc-in (parse inner (assoc-in s [:state :symbols] symbols))
              [:state :symbols]
              (-> s :state :symbols))))

(defn grammar
  "Constructs a parser from a map of symbol names to parsers.
  The optional second argument (`start`) gives the symbol where parsing starts.
  The default start symbol is `:start`."
  ([symbols] (grammar symbols :start))
  ([symbols start]
   {:type :grammar :symbols symbols :start start}))

;(defn with-actions
;  "Given a grammar and a map of symbol names to actions, wraps the corresponding
;  parsers with those actions.
;  Actions are called with (action value loc), and should either return a new
;  value or throw."
;  [g actions]
;  (update g :symbols #(merge-with with-actions % actions)))

(defn parse-str
  "The main entry point for parsing.
  Takes a parser `p`, a source `filename` (only for error messages)
  and the `input` text, attempts to parse the entire string with the parser.
  If there is any input left at the end, parsing fails.
  On successful, complete parsing, returns the parsed value."
  ([p filename input]
   (parse-str p filename input {}))
  ([p filename input state]
   ; The :state on the stream is always a map; the user's opaque state value is
   ; attached at :user.
   (let [s0 (st/stream filename input {:user state})
         s1 (parse p s0)]
     (if (st/at-eof? s1)
       (:value s1)
       (errs/parse-error
         s1 (apply str "unexpected trailing output: "
                   (take 20 (st/remaining-input s1))))))))

