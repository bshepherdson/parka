(ns parka.json-test
  "Test that builds and exercises a JSON parser using this library."
  (:require
    [clojure.test :refer [deftest is testing]]
    [parka.core :as p]))

(def json-rules
  {:start      (p/between :ws :json-value)
   :json-value (p/alt :array :object :null :bool :string :number)
   :ws         (p/many-drop (p/one-of " \t\r\n"))
   :null       (p/with-action "null" (constantly nil))
   :bool       (p/with-action
                 (p/alt "true" "false")
                 (fn [v _] (= v "true")))
   :hex        (p/one-of "0123456789ABCDEFabcdef")
   :unicode    (p/with-action (p/pseq "\\u" :hex :hex :hex :hex)
                 (fn [[_ a b c d] _]
                   (char (Long/parseLong (str a b c d) 16))))
   :escaped    (p/with-action (p/pseq-at 1 "\\" (p/one-of "\"\\/nrbft"))
                 (fn [v _]
                   (case v
                     \n \newline
                     \r \return
                     \b \backspace
                     \f \formfeed
                     \t \tab
                     v)))
   :strchar    (p/alt :unicode :escaped p/any)
   :string     (p/stringify (p/pseq-at 1 "\""
                                       (p/many-till :strchar "\"")))

   :pm         (p/one-of "+-")
   :digits     (p/stringify (p/many1 (p/span \0 \9)))
   :number     (p/with-action
                 (p/pseq (p/optional :pm) :digits
                         (p/optional (p/pseq-at 1 "." :digits))
                         (p/optional (p/pseq (p/lit-ic "e")
                                             (p/optional :pm)
                                             :digits)))
                 (fn [[sign main d [_ em e]] _]
                   (let [n (Integer/parseInt main)
                         pos (if (or d e)
                               (Double/parseDouble
                                 (str n
                                      (if d (str "." d) "")
                                      (if e (str "e" (or em "") e) "")))
                               (Integer/parseInt main))]
                     (if (= \- sign)
                       (- pos)
                       pos))))

   :comma      (p/pseq :ws "," :ws)
   :object     (p/with-action
                 (p/between
                   "{" "}"
                   (p/between :ws (p/sep-by :key-value :comma)))
                 (fn [kvs _]
                   (into {} kvs)))
   :key-value  (p/with-action
                 (p/pseq :string :ws ":" :ws :json-value)
                 (fn [[k _ _ _ v] _] [k v]))
   :array      (p/with-action
                 (p/between
                   "[" "]"
                   (p/between :ws (p/sep-by :json-value :comma)))
                 (fn [vs _] (vec vs)))})

(defn test-parse [input]
  (p/parse-str (p/grammar json-rules) "<test>" input))

(defn test-parse-from [sym input]
  (p/parse-str (p/grammar json-rules sym) "<test>" input))

(deftest test-number
  (is (= 1        (test-parse-from :number "1")))
  (is (= 123      (test-parse-from :number "0123")))
  (is (= 0        (test-parse-from :number "0")))
  (is (= 88       (test-parse-from :number "+88")))
  (is (= -41      (test-parse-from :number "-41")))
  (is (= 0.02     (test-parse-from :number "0.02")))
  (is (= 200.0    (test-parse-from :number "0.02e4")))
  (is (= 123.4e-2 (test-parse-from :number "123.4e-2")))
  (is (= 0.12     (test-parse-from :number "12e-2")))
  )

(deftest test-ws
  (is (= nil (test-parse-from :ws "   \t\t \n   \t"))))

(deftest test-comma
  (is (= [nil "," nil] (test-parse-from :comma "    \t  ,\n   "))))

(deftest test-string
  (testing "basic strings"
    (is (= "asdf" (test-parse-from :string "\"asdf\"")))
    (is (= ""     (test-parse-from :string "\"\""))))

  (testing "escapes"
    (is (= \\         (test-parse-from :strchar "\\\\")))
    (is (= \"         (test-parse-from :strchar "\\\"")))
    (is (= \newline   (test-parse-from :strchar "\\n")))
    (is (= \return    (test-parse-from :strchar "\\r")))
    (is (= \formfeed  (test-parse-from :strchar "\\f")))
    (is (= \backspace (test-parse-from :strchar "\\b")))
    (is (= \tab       (test-parse-from :strchar "\\t"))))
  (testing "unicode"
    (is (= \uBEEF (test-parse-from :strchar "\\ubeef"))))
  (testing "strings with escapes"
    (is (= "\n"   (test-parse-from :string "\"\\n\"")))
    (is (= "\\"   (test-parse-from :string "\"\\\\\"")))))

(deftest test-bool
  (is (= false (test-parse-from :bool "false")))
  (is (= true  (test-parse-from :bool "true"))))

(deftest test-null
  (is (= nil (test-parse-from :null "null"))))

(deftest test-array
  (is (= [] (test-parse-from :array "[]")))
  (is (= [] (test-parse-from :array "[  ]")))
  (is (= ["abc" "def" false]
         (test-parse-from :array "[  \"abc\", \"def\"  ,false  ]"))))

(deftest test-object
  (is (= {} (test-parse-from :object "{}")))
  (is (= {} (test-parse-from :object "{   }")))
  (is (= {"abc" "def"
          "ghi" 77}
         (test-parse-from :object
                          "{\"abc\": \"def\"  ,  \n  \"ghi\"   :77 }"))))

(deftest test-json-parser
  (is (= 7     (test-parse "7")))
  (is (= -91   (test-parse "-91")))
  (is (= nil   (test-parse "null")))
  (is (= false (test-parse "false")))
  (is (= true  (test-parse "true")))
  (is (= []    (test-parse "[]")))
  (is (= {}    (test-parse "{}")))

  (is (= [1 false nil {"abc"  true
                       "def"  "yolo"
                       "asdf" nil}]
         (test-parse "  [ 1  ,false,null , {  \"abc\"  :true, \"def\": \"yolo\", \"asdf\": null  }  ]  "))))


(comment
  ; Running this JSON parser on the 7MB payload used by
  ; https://github.com/GoogleChromeLabs/json-parse-benchmark
  ; This times 100 runs.
  (time (test-parse (slurp "./inspector-json-payload.json")))
  (time (test-parse (slurp "./pp.json")))
  )

