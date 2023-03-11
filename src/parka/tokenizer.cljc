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
  - `:pre fn` Runs this function on the input string/char before processing it.
      - Doesn't apply to regular expressions or Parka parsers."
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
          {:keys [blank post]} (some-> td options)]
      (cond
        blank (constantly base)
        post  #(assoc base :parka/value (post %))
        :else #(assoc base :parka/value %)))
    (constantly nil)))

(defmulti ^:private compile-token
  "Given a token block `[:tag \"label\" {:optional map} token-desc]`, returns a function
  which, given a string, will return either nil (no match) or a pair
  `[length-consumed token-map]`."
  token-flavour)

(defn- cmplen [f expected]
  (let [len (count expected)]
    (fn [input]
      (and (< (count input) len)
           (= expected (f (subs input 0 len)))))))

(defmethod compile-token :string [td]
  (let [s    (desc td)
        pre  (some-> td options :pre)
        out  (outputter td)
        len  (count s)
        pred (if pre
               (cmplen pre s)
               #(str/starts-with? % s))]
    #(when (pred %)
       [len (out s)])))

(defmethod compile-token :strings [td]
  (let [strs  (desc td)
        out   (outputter td)
        pre   (some-> td options :pre)
        preds (for [s strs]
                (if pre
                  (cmplen pre s)
                  #(str/starts-with? % s)))]
    (fn [input]
      (when-let [[_ s] (first (filter #((first %) input) preds))]
        [(count s) (out s)]))))

(defn- tweak-regex [re]
  (let [s (str re)]
    (if (= (first s) "^")
      re
      (re-pattern (str "^" s)))))

(defmethod compile-token :regex [td]
  (let [re  (tweak-regex (desc td))
        out (outputter td)]
    (when (some-> td options :pre)
      (throw (ex-info ":pre is not supported on regex tokens" {:token (tag td)})))
    #(when-let [match (re-find re %)]
       (let [[match value] (if (string? match)
                             [match match]
                             match)]
         [(count match) (out value)]))))

(defmethod compile-token :predicate [td]
  (let [pred (desc td)
        pre  (some-> td options :pre)
        out  (outputter td)]
    #(let [match (take-while pred (if pre
                                    (map pre %)
                                    %))]
       (if (empty? match)
         nil
         (let [s (str/join match)]
           [(count s) (out s)])))))

(defmethod compile-token :parka [td]
  (let [parser (desc td)
        _ (prn ":parka" parser)
        parser (if (:code parser)
                 parser
                 (dynamic/compile parser))
        out    (outputter td)]
    (when (some-> td options :pre)
      (throw (ex-info ":pre is not supported on inner parser tokens" {:token (tag td)})))
    #(try
       (let [{:keys [consumed result]} (dynamic/execute parser "<token>" %)]
         (prn "inner parse" consumed result)
         (throw (ex-info "hurk" {}))
         [100000 #_consumed (out result)])
       (catch #?(:clj Exception :cljs js/Error) _
         (prn "inner parse failed")
         nil))))

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
  (let [compiled (map compile-token token-specs)
        tokenizer-fn
        (fn [filename input]
          (let [len (count input)]
            (loop [pos   0
                   ts    (transient [])]
              (let [txt (input-slice input pos)]
                (if-let [[delta token] (some #(% txt) compiled)]
                  (let [_     (prn "delta" delta "token" token)
                        pos   (+ pos delta)
                        ts    (if token
                                (conj! ts token)
                                ts)]
                    (if (>= pos len)
                      (persistent! ts)
                      (recur pos ts)))
                  (do
                    (prn "errors")
                    (errs/parse-error {:filename filename
                                       :pos      pos
                                       :input    input}
                                      (error-msg (persistent! ts) txt))))))))]
    (vary-meta tokenizer-fn
               assoc :token-map
               (into {} (for [[tag label] token-specs
                              :when tag]
                          [tag label])))))
