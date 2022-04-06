(ns parka.json-test
  "Test that builds and exercises a JSON parser using this library."
  (:require
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(def json-rules
  {:start      (p/keep :json-value [:ws :json-value :ws p/eof])
   :json-value (p/alt :array :object :null :bool :string :number)
   :ws         (p/* (p/one-of " \t\r\n"))
   :null       (p/action "null" (constantly nil))
   :bool       (p/action
                 (p/alt "true" "false")
                 #(= % "true"))
   :hex        (p/one-of "0123456789ABCDEFabcdef")
   :unicode    (p/action ["\\u" :hex :hex :hex :hex]
                 (fn [{[a b c d] :hex}]
                   (char (Long/parseLong (str a b c d) 16))))
   :escaped    (p/action ["\\" (p/one-of "\"\\/nrbft")]
                 (fn [[_ v]]
                   (case v
                     \n \newline
                     \r \return
                     \b \backspace
                     \f \formfeed
                     \t \tab
                     v)))
   :strchar    (p/alt :unicode :escaped [(p/not \") p/any])
   :string     [\" (p/* :strchar) \"]
   #_(p/stringify (p/pseq-at 1 "\""
                                       (p/many-till :strchar "\"")))

   :pm         (p/one-of "+-")
   :digits     (p/+ (p/one-of "0123456789"))
   #_(p/stringify (p/many1 (p/span \0 \9)))
   :number     (p/action
                 [(p/? :pm)
                  :digits
                  (p/? (p/keep :digits ["." :digits]))
                  (p/? [#{\e \E}
                        {:pm (p/? :pm)}
                        :digits])]
                 (fn [res]
                   (prn res)
                   0
                   #_(let [n (Integer/parseInt main)
                         pos (if (or d e)
                               (Double/parseDouble
                                 (str n
                                      (if d (str "." d) "")
                                      (if e (str "e" (or em "") e) "")))
                               (Integer/parseInt main))]
                     (if (= \- sign)
                       (- pos)
                       pos))))

   :comma      [:ws "," :ws]
   :object     (p/action [\{ :ws
                          :key-value
                          {:tail (p/* (p/keep :key-value [:comma :key-value]))}
                          :ws \}]
                 (fn [{:keys [key-value tail]}]
                   (reduce (into {} key-value) tail)))
   :key-value  (p/action
                 [:string :ws ":" :ws :json-value]
                 (fn [{:keys [string json-value]}]
                   [string json-value]))
   :array      (p/keep :array-guts [\[ :ws :array-guts :ws \]])
   :array-guts (p/alt [:json-value
                       {:tail (p/* (p/keep :json-value [:comma :json-value]))}]
                      (p/and \]))})

(defn test-parse [input]
  (p/parse (p/compile (p/grammar json-rules :start)) "<test>" input))

(defn test-parse-from [sym input]
  (p/parse (p/compile (p/grammar json-rules sym)) "<test>" input))

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

