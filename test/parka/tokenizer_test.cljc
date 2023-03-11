(ns parka.tokenizer-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [are deftest is testing]]
   [parka.api :as p]
   [parka.tokenizer :as pt]))

(defn- parse-int [text base]
  (#?(:clj Long/parseLong :cljs js/parseInt) text base))

(def ^:private escapes
  {"\\\"" "\""
   "\\\\" "\\"
   "\\n"  "\n"
   "\\r"  "\r"
   "\\b"  "\b"
   "\\f"  "\f"
   "\\t"  "\t"})

(defn- string-escapes [s]
  (-> s
      ;; Unicode \u1234 escapes
      (str/replace #"\\u([0-9a-fA-F]{4})" #(char (parse-int % 16)))
      ;; ASCII \_ escapes
      (str/replace #"\\[\"\\/bfnrt]"
                   #(get escapes %))))

(def json-tokens
  [[nil      "whitespace" (set " \t\r\n")]
   [:lbrace  "{" "{"]
   [:rbrace  "}" "}"]
   [:lsquare "[" "["]
   [:rsquare "]" "]"]
   [:colon   ":" ":"]
   [:comma   "," ","]
   [:null    "null"    {:post (constantly nil)} "null"]
   [:boolean "boolean" {:post #(= % "true")}    ["true" "false"]]
   [:lit/string "string"
    {:post string-escapes}
    #"^\"(|[^\n]*?[^\\])\""]
   [:lit/double "number"
    {:post #(#?(:clj Double/parseDouble :cljs js/parseFloat) %)}
    #"^[+-]?\d+(?:\.\d+[eE][+-]?\d+|\.\d+|[eE][+-]?\d+)"]
   [:lit/integer "number"
    {:post #(#?(:clj Integer/parseInt :cljs js/parseInt) %)}
    (set "0123456789")]])

(def json-tokenizer
  (pt/tokenizer json-tokens))

(def json-labels
  (into {} (map #(vec (take 2 %)) json-tokens)))

(defn- tt [tag text]
  #:parka{:token tag :label (json-labels tag) :value text})

(deftest exact-strings-test
  (is (= [(tt :lbrace  "{")
          (tt :lbrace  "{")
          (tt :rbrace  "}")
          (tt :rsquare "]")
          (tt :lsquare "[")
          (tt :lbrace  "{")]
         (json-tokenizer "<test>" "{{}][{"))))

(deftest string-list-test
  (is (= [(tt :boolean true)]  (json-tokenizer "<test>" "true")))
  (is (= [(tt :boolean false)] (json-tokenizer "<test>" "false"))))

(deftest regex-test
  (are [exp input] (= [(tt :lit/string exp)] (json-tokenizer "<test>" input))
    "foo"                                         "\"foo\""
    "foo bar baz"                                 "\"foo bar baz\""
    "nested \"quotes\""                           "\"nested \\\"quotes\\\"\""
    "more \"complex \\\"nesting \\\" here\" gosh" "\"more \\\"complex \\\\\"nesting \\\\\" here\\\" gosh\""
    "new\nlines"                                  "\"new\\nlines\""
    "more\bescape\tcharacters\f"                  "\"more\\bescape\\tcharacters\\f\"")

  (are [exp input] (= [(tt :lit/double exp)] (json-tokenizer "<test>" input))
    1.23 "1.23"
    1e4 "1e4"
    1234e-2 "1234e-2"
    12.34e-99 "12.34e-99"
    12.34e+0 "12.34e+0"))

(deftest set-test
  (are [exp input] (= [(tt :lit/integer exp)] (json-tokenizer "<text>" input))
    7 "7"
    0 "0000"
    117 "117"
    281921 "281921"))

(deftest document-test
  (let [input  (str "{   \"foo\" :77, \n\n   \"bar\":[  7.7,  false   ]"
                    "  ,\n\n  \"things\"  :true } \n  ")]
    (is (= [(tt :lbrace      "{")
            (tt :lit/string  "foo")
            (tt :colon       ":")
            (tt :lit/integer 77)
            (tt :comma       ",")
            (tt :lit/string  "bar")
            (tt :colon       ":")
            (tt :lsquare     "[")
            (tt :lit/double  7.7)
            (tt :comma       ",")
            (tt :boolean     false)
            (tt :rsquare     "]")
            (tt :comma       ",")
            (tt :lit/string  "things")
            (tt :colon       ":")
            (tt :boolean     true)
            (tt :rbrace      "}")]
           (json-tokenizer "<file>" input))))
  (is (= #:parka{:parse-error true,
                 :loc ["<file>" "{}\"abc\"   !" 10],
                 :message (str "Tokenizing failure: could not match input \"!\"\n"
                               "\tLast few tokens: { (\"{\" \")\", } (\"}\" \")\", string (\"abc\" \")\"")}
         (json-tokenizer "<file>" "{}\"abc\"   !"))))

(comment
  (parka.dynamic/execute (p/compile (p/* "ab")) "<test>" "abababab"))

(deftest inner-parser-tokenizing-test
  (let [tokens    [[:abs "AB pairs" (p/* "ab")]]
        tokenizer (pt/tokenizer tokens)]
    (tokenizer "<text>" "abababab")
    #_(is (= [#:parka{:token :abs :label "AB pairs" :value ["ab" "ab" "ab" "ab"]}]
             (tokenizer "<text>" "abababab"))))

  #_(let [inner-grammar {:string      (p/action (p/pick [1] [\" (p/* :string-char) \"])
                                                str/join)
                         :string-char (p/alt :unicode :escape (p/any-but \"))
                         :unicode     (p/action ["\\u" :hex :hex :hex :hex]
                                                (fn [[_ & hex]]
                                                  (char (parse-int (str/join hex) 16))))
                         :hex         (set "0123456789ABCDEFabcdef")
                         :escape      (p/action ["\\" p/any]
                                                #(get escapes (apply str %)))}
          inner-parser  (p/compile (p/grammar inner-grammar :string))
          tokens        [[:string "string" inner-parser]]
          tokenizer     (pt/tokenizer tokens)
          tt            (fn [value]
                          #:parka{:token :string
                                  :label "string"
                                  :value value})]
      (is (= [(tt "foo")] (tokenizer "<test>" "\"foo\"")))))

;; TODO: Test parser-based tokenization
;; TODO: Test :pre for doing things like case-insensitive tokens.
