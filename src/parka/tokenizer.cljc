(ns parka.tokenizer
  "Routines and helpers for tokenizing an input stream.
  The tokens for a parser are defined as an *ordered* list of
  `[:token-tag \"human-friendly label\" {:optional/map \"of options\"} token-spec]`
  tuples.

  There are several ways to define the `token-spec`:
  - A string, which matches literally and exactly.
  - A sequence of strings, which are checked in order and matched likewise.
  - A function (often a set) which is sent *single characters* one at a time.
    This works like `take-while`, except that of course if the first character
    fails to match, the token is skipped. It will not match an empty string.
  - A regular expression, which is given the entire input string from the
    current point, and captures whatever it matches.
    If the regex contains capture groups, the first one is used as the value.
    If it has no capture groups, the whole regex match is used instead.
  - A parser defined using the syntax of Parka, but treating characters as
    tokens. Like the function and regex, it has to consume something to
    generate a token.

  Remember that the tokens are checked in order! Put symbols and keywords
  towards the top and general identifiers near the bottom. Also watch out for
  operators that are prefixes of others, eg. `++` should be checked before `+`.

  All characters are visible to the tokenizer, nothing is skipped.
  If the `token-tag` is `nil` rather than a keyword, no token will be generated.
  This can be used to skip whitespace.

  Options:
  - `:blank true` Drops the input value, for tokens like punctuation with no value.
  - `:post fn` Tokenizer will run `(update t :value fn)` to post-process the value.
  "
  (:require
   [clojure.string :as str]
   [parka.dynamic :as dynamic]
   [parka.errors :as errs]
   #_[parka.machine.compiler :as compiler]
   #_[parka.machine.peg :as engine])
  #?(:clj (:import java.nio.CharBuffer)))

(defn- regex? [x]
  (instance? #?(:clj  java.util.regex.Pattern
                :cljs js/RegExp)
             x))

(defn- tag [[t _ _]]
  t)

(defn- label [[_ l _]]
  l)

(defn- options [td]
  (when (> (count td) 3)
    (nth td 2)))

(defn- desc [td]
  (last td))

(defn- token-flavour [token-desc]
  (let [t (desc token-desc)]
    (cond
      (string?     t) :string
      (sequential? t) :strings
      (regex? t)      :regex
      (map? t)        :parka
      (or (fn? t)
          (set? t))   :predicate
      :else           (throw (ex-info "unknown type of token" token-desc)))))

(defn- outputter [td]
  (if-let [token (tag td)]
    (let [base #:parka{:token token
                       :label (label td)}
          post (some-> td options :post)]
      (if post
        #(assoc base :parka/value (post %))
        #(assoc base :parka/value %)))
    (constantly nil)))

(defmulti ^:private compile-token
  "Given a token block `[:tag \"label\" {:optional map} token-desc]`, returns a function
  which, given a string, will return either nil (no match) or a pair
  `[length-consumed token-map]`."
  token-flavour)

;; TODO: Case-insensitive mode for :string and :strings.
(defmethod compile-token :string [td]
  (let [s   (desc td)
        out (outputter td)]
    #(when (str/starts-with? % s)
       [(count s) (out s)])))

(defmethod compile-token :strings [td]
  (let [strs (desc td)
        out  (outputter td)]
    (fn [input]
      (when-let [s (first (filter #(str/starts-with? input %) strs))]
        [(count s) (out s)]))))

(defn- tweak-regex [re]
  (let [s (str re)]
    (if (= (first s) "^")
      re
      (re-pattern (str "^" s)))))

(defmethod compile-token :regex [td]
  (let [re  (tweak-regex (desc td))
        out (outputter td)]
    #(when-let [match (re-find re %)]
       (let [[match value] (if (string? match)
                             [match match]
                             match)]
         [(count match) (out value)]))))

(defmethod compile-token :predicate [td]
  (let [pred (desc td)
        out  (outputter td)]
    #(let [match (take-while pred %)]
       (if (empty? match)
         nil
         (let [s (str/join match)]
           [(count s) (out s)])))))

(defmethod compile-token :parka [td]
  (let [parser (dynamic/compile (desc td))
        out    (outputter td)]
    #(let [res (dynamic/evaluate parser "<token>" %)]
       (when-not (:error res)
         [(:pos res) (out (peek (:caps res)))]))))

(defn- input-slice [input pos]
  ;; In CLJS, substrings are constant-time so we can just use that.
  ;; In CLJ, String.substring makes a copy, so we use CharBuffer.wrap().
  #?(:cljs (.substring input pos)
     :clj  (CharBuffer/wrap input pos (count input))))

(defn- error-msg [tokens text]
  (let [tmsg (->> (take-last 3 tokens)
                  (map #(str (:parka/label %) " (" (pr-str (:parka/value %) ")")))
                  (str/join ", "))
        c20  (str/join (take 20 text))]
    (str "Tokenizing failure: could not match input \"" c20 "\"\n"
         "\tLast few tokens: " tmsg)))

(defn tokenizer
  "Given a sequence of tokenizers, compiles them and returns a function from an input
  string to a sequence of tokens."
  [token-specs]
  (let [compiled (map compile-token token-specs)]
    (let [tokenizer-fn
          (fn [filename input]
            (let [len (count input)]
              (loop [pos   0
                     ts    (transient [])]
                (let [txt (input-slice input pos)]
                  (if-let [[delta token] (some #(% txt) compiled)]
                    (let [pos   (+ pos delta)
                          ts    (if token
                                  (conj! ts token)
                                  ts)]
                      (if (>= pos len)
                        (persistent! ts)
                        (recur pos ts)))
                    (errs/parse-error {:filename filename
                                       :pos      pos
                                       :input    input}
                                      (error-msg (persistent! ts) txt)))))))]
      (vary-meta tokenizer-fn
                 assoc :token-map
                 (into {} (for [[tag label] token-specs
                                :when tag]
                            [tag label]))))))
