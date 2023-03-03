(ns parka.json-test
  "Test that builds and exercises a (very imperfect!) JSON parser using this library.

  Don't use these parsers for real JSON. It doesn't handle string escapes perfectly,
  among other weaknesses."
  (:require
   [clojure.string :as string]
   [clojure.test :refer [are deftest is testing]]
   [parka.api :as p]
   [parka.tokenizer-test :as tok-test]))

(def json-char-rules
  {:start      (p/pick [1] [:ws :json-value :ws p/eof])
   :json-value (p/alt :array :object :null :boolean :string :number)
   :ws         (p/drop (p/* (set " \t\r\n")))
   :null       (p/action "null" (constantly nil))
   :boolean    (p/action
                (p/alt "true" "false")
                #(= % "true"))
   :hex        (set "0123456789ABCDEFabcdef")
   :unicode    (p/action ["\\u" :hex :hex :hex :hex]
                         (fn [[_ a b c d]]
                           (str (char (Long/parseLong (str a b c d) 16)))))
   :escaped    (p/action ["\\" (set "\"\\/nrbft")]
                         (fn [[_ v]]
                           (case v
                             "n" "\n"
                             "r" "\r"
                             "b" "\b"
                             "f" "\f"
                             "t" "\t"
                             v)))
   :strchar    (p/alt :unicode :escaped (p/pick [1] [(p/not \") p/any]))
   :string-guts (p/* :strchar)
   :rawstring  [\" (p/* :strchar) \"]
   :string     (p/action :rawstring
                         (comp string/join second))
   :pm         (set "+-")
   :digits     (p/str (p/+ (p/span \0 \9)))
   :number     (p/action
                [(p/? :pm)
                 :digits
                 (p/? (p/pick [1] ["." :digits]))
                 (p/? [#{\e \E}
                       (p/? :pm)
                       :digits])]
                (fn [[pm main decimal [_ em exp]]]
                  (let [n (Integer/parseInt (string/join main))
                        pos (if (or decimal exp)
                              (Double/parseDouble
                               (str n
                                    (if decimal
                                      (str "." decimal)
                                      "")
                                    (if exp
                                      (str "e" (or em "") exp)
                                      "")))
                              (Integer/parseInt main))]
                    (if (= "-" pm)
                      (- pos)
                      pos))))

   :comma       [:ws "," :ws]
   :object      (p/alt :empty-obj :full-obj)
   :empty-obj   (p/action [\{ :ws \}] (constantly {}))
   :full-obj    (p/pick [2] [\{ :ws :obj-guts :ws \}])
   :obj-guts    (p/action [:key-value (p/* (p/pick [1] [:comma :key-value]))]
                          (fn [[x xs]]
                            (into {} (concat [x] xs))))
   :key-value   (p/action
                 [:string :ws ":" :ws :json-value]
                 (fn [[k _ _ _ v]]
                   [k v]))
   :array       (p/pick [2] [\[ :ws :array-guts :ws \]])
   :array-guts  (p/alt :full-array :empty-array)
   :full-array  (p/action
                 [:json-value (p/* (p/pick [1] [:comma :json-value]))]
                 (fn [[x xs]]
                   (into [x] xs)))
   :empty-array (p/action
                 (p/and \]) ; ws has already been consumed here.
                 (constantly []))})

(defn chars-test-parse-from [sym input]
  (p/parse (p/compile (p/grammar json-char-rules sym)) "<test>" input))

(defn chars-test-parse [input]
  (chars-test-parse-from :start input))

(deftest test-number
  (are [exp input] (= exp (chars-test-parse-from :number input))
    1        "1"
    123      "0123"
    0        "0"
    88       "+88"
    -41      "-41"
    0.02     "0.02"
    200.0    "0.02e4"
    123.4e-2 "123.4e-2"
    0.12     "12e-2"))

(deftest test-string
  (testing "basic strings"
    (is (= "asdf" (chars-test-parse-from :string "\"asdf\"")))
    (is (= ""     (chars-test-parse-from :string "\"\""))))

  (testing "escapes"
    (are [exp input] (= exp (chars-test-parse-from :string input))
      "\\" "\"\\\\\""
      "\"" "\"\\\"\""
      "\n" "\"\\n\""
      "\r" "\"\\r\""
      "\f" "\"\\f\""
      "\b" "\"\\b\""
      "\t" "\"\\t\""))

  (testing "unicode"
    (is (= "\uBEEF"   (chars-test-parse-from :string "\"\\ubeef\""))))

  (testing "strings with escapes"
    (is (= "\n"   (chars-test-parse-from :string "\"\\n\"")))
    (is (= "\\"   (chars-test-parse-from :string "\"\\\\\"")))))

(deftest test-bool
  (is (= false (chars-test-parse-from :boolean "false")))
  (is (= true  (chars-test-parse-from :boolean "true"))))

(deftest test-null
  (is (= nil (chars-test-parse-from :null "null"))))

(deftest test-array
  (are [exp input] (= exp (chars-test-parse-from :array input))
    []                  "[]"
    []                  "[  ]"
    ["abc" "def" false] "[  \"abc\", \"def\"  ,false  ]"))

(deftest test-object
  (are [exp input] (= exp (chars-test-parse-from :object input))
    {}           "{}"
    {}           "{   }"
    {"abc" "def"
     "ghi" 77}   "{\"abc\": \"def\"  ,  \n  \"ghi\"   :77 }"))

(deftest test-json-parser
  (are [exp input] (= exp (chars-test-parse-from :start input))
    7               "7"
    -91             "-91"
    nil             "null"
    false           "false"
    true            "true"
    []              "[]"
    {}              "{}"
    [1 false nil
     {"abc"  true
      "def"  "yolo"
      "asdf" nil}]  "  [ 1  ,false,null , {  \"abc\"  :true, \"def\": \"yolo\", \"asdf\": null  }  ]  "))

;;; ------------------------ Tokenized parser tests --------------------------
;; Borrowing the tokens from parka.tokenizer-test, and wrapping them up into a parser.
(def json-token-rules
  {:start       (p/pick [0] [:json-value p/eof])
   :json-value  (p/alt :array :object :null :boolean :string :number)
   :number      (p/alt :lit/double :lit/integer)
   :string      :lit/string
   :object      (p/alt :empty-obj :full-obj)
   :empty-obj   (p/action [:lbrace :rbrace]
                          (constantly {}))
   :full-obj    (p/pick [1] [:lbrace :obj-guts :rbrace])
   :obj-guts    (p/action [:key-value (p/* (p/pick [1] [:comma :key-value]))]
                          (fn [[x xs]]
                            (into {} (concat [x] xs))))
   :key-value   (p/action
                 [:lit/string :colon :json-value]
                 (fn [[k _ v]]
                   [k v]))
   :array       (p/pick [1] [:lsquare :array-guts :rsquare])
   :array-guts  (p/alt :full-array :empty-array)
   :full-array  (p/action
                 [:json-value (p/* (p/pick [1] [:comma :json-value]))]
                 (fn [[x xs]]
                   (into [x] xs)))
   :empty-array (p/action
                 (p/and :rsquare) ; ws has already been consumed here.
                 (constantly []))})

(defn tokens-test-parse-from [sym input]
  (p/parse (p/compile (p/grammar json-token-rules sym)
                      tok-test/json-tokenizer)
           "<test>" input))

;; Note that this token-based parser is n
(deftest test-number-tokens
  (are [exp input] (= exp (tokens-test-parse-from :number input))
    1        "1"
    123      "0123"
    0        "0"
    88.2     "+88.2" ;; The :lit/integer token doesn't support +/- for test-y reasons.
    -41.7    "-41.7"
    0.02     "0.02"
    200.0    "0.02e4"
    123.4e-2 "123.4e-2"
    0.12     "12e-2"))

(deftest test-string-tokens
  (testing "basic strings"
    (is (= "asdf" (tokens-test-parse-from :string "\"asdf\"")))
    (is (= ""     (tokens-test-parse-from :string "\"\""))))

  #_(testing "escapes"
      (are [exp input] (= exp (tokens-test-parse-from :string input))
        "\\" "\"\\\\\""
        "\"" "\"\\\"\""
        "\n" "\"\\n\""
        "\r" "\"\\r\""
        "\f" "\"\\f\""
        "\b" "\"\\b\""
        "\t" "\"\\t\""))

  #_(testing "unicode"
      (is (= "\uBEEF"   (tokens-test-parse-from :string "\"\\ubeef\""))))

  #_(testing "strings with escapes"
      (is (= "\n"   (tokens-test-parse-from :string "\"\\n\"")))
      (is (= "\\"   (tokens-test-parse-from :string "\"\\\\\"")))))

(deftest test-bool-tokens
  (is (= false (tokens-test-parse-from :json-value "false")))
  (is (= true  (tokens-test-parse-from :json-value "true"))))

(deftest test-null-tokens
  (is (= nil (tokens-test-parse-from :json-value "null"))))

(deftest test-array-tokens
  (are [exp input] (= exp (tokens-test-parse-from :array input))
    []                  "[]"
    []                  "[  ]"
    ["abc" "def" false] "[  \"abc\", \"def\"  ,false  ]"))

(deftest test-object-tokens
  (are [exp input] (= exp (tokens-test-parse-from :object input))
    {}           "{}"
    {}           "{   }"
    {"abc" "def"
     "ghi" 77}   "{\"abc\": \"def\"  ,  \n  \"ghi\"   :77 }"))

(deftest test-json-parser-tokens
  (are [exp input] (= exp (tokens-test-parse-from :start input))
    7               "7"
    -91.23          "-91.23"
    nil             "null"
    false           "false"
    true            "true"
    []              "[]"
    {}              "{}"
    [1 false nil
     {"abc"  true
      "def"  "yolo"
      "asdf" nil}]  "  [ 1  ,false,null , {  \"abc\"  :true, \"def\": \"yolo\", \"asdf\": null  }  ]  "))

(comment
  ; Running this JSON parser on the 7MB payload used by
  ; https://github.com/GoogleChromeLabs/json-parse-benchmark
  ; This times 100 runs.
  (time (chars-test-parse (slurp "./inspector-json-payload.json")))
  (time (chars-test-parse (slurp "./pp.json"))))
