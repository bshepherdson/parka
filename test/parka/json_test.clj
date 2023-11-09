(ns parka.json-test
  "Test that builds and exercises a JSON parser using this library."
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(def json-rules
  {:start      (p/value [:ws :json-value :ws p/eof] second)
   :json-value (p/alt :array :object :null :bool :string :number)
   :ws         (p/drop (p/* (p/one-of " \t\r\n")))
   :null       (p/action "null" (constantly nil))
   :bool       (p/action
                 (p/alt "true" "false")
                 #(= % "true"))
   :hex        (p/one-of "0123456789ABCDEFabcdef")
   :unicode    (p/action ["\\u" :hex :hex :hex :hex]
                 (fn [[_ a b c d]]
                   (str (char (Long/parseLong (str a b c d) 16)))))
   :escaped    (p/action ["\\" (p/one-of "\"\\/nrbft")]
                 (fn [[_ v]]
                   (case v
                     "n" "\n"
                     "r" "\r"
                     "b" "\b"
                     "f" "\f"
                     "t" "\t"
                     v)))
   :strchar    (p/alt :unicode :escaped (p/do (p/not \") p/any))
   :string     (p/action [\" (p/* :strchar) \"]
                         (comp string/join second))
   :pm         (p/one-of "+-")
   :digits     (p/str (p/+ (p/one-of "0123456789")))
   :number     (p/action
                 [(p/? :pm)
                  :digits
                  (p/? (p/do "." :digits))
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
   :full-obj    (-> :obj-guts
                    (p/between :ws)
                    (p/between \{ \}))
   :obj-guts    (p/action [:key-value (p/* (p/do :comma :key-value))]
                          (fn [[x xs]]
                            (into {} (concat [x] xs))))
   :key-value   (p/action
                  [:string :ws ":" :ws :json-value]
                  (fn [[k _ _ _ v]]
                    [k v]))
   :array       (-> (p/alt :full-array :empty-array)
                    (p/between :ws)
                    (p/between \[ \]))
   :full-array  (p/action
                 [:json-value (p/* (p/do :comma :json-value))]
                 (fn [[x xs]]
                   (into [x] xs)))
   :empty-array (p/action
                  (p/and \]) ; ws has already been consumed here.
                  (constantly []))})

(defn test-parse-from [sym input]
  (let [result (p/parse (p/compile (p/grammar json-rules sym)) "<test>" input)]
    (if (contains? result :success) ; nil is a valid result!
      (:success result)
      result)))

(defn test-parse [input]
  (test-parse-from :start input))

(deftest test-number
  (is (= 1        (test-parse-from :number "1")))
  (is (= 123      (test-parse-from :number "0123")))
  (is (= 0        (test-parse-from :number "0")))
  (is (= 88       (test-parse-from :number "+88")))
  (is (= -41      (test-parse-from :number "-41")))
  (is (= 0.02     (test-parse-from :number "0.02")))
  (is (= 200.0    (test-parse-from :number "0.02e4")))
  (is (= 123.4e-2 (test-parse-from :number "123.4e-2")))
  (is (= 0.12     (test-parse-from :number "12e-2"))))

(deftest test-ws
  (is (= nil (test-parse-from :ws "   \t\t \n   \t"))))

(deftest test-comma
  (is (= [nil "," nil] (test-parse-from :comma "    \t  ,\n   "))))

(deftest test-string
  (testing "basic strings"
    (is (= "asdf" (test-parse-from :string "\"asdf\"")))
    (is (= ""     (test-parse-from :string "\"\""))))

  (testing "escapes"
    (is (= "\\"       (test-parse-from :strchar "\\\\")))
    (is (= "\""       (test-parse-from :strchar "\\\"")))
    (is (= "\n"       (test-parse-from :strchar "\\n")))
    (is (= "\r"       (test-parse-from :strchar "\\r")))
    (is (= "\f"       (test-parse-from :strchar "\\f")))
    (is (= "\b"       (test-parse-from :strchar "\\b")))
    (is (= "\t"       (test-parse-from :strchar "\\t"))))
  (testing "unicode"
    (is (= "\uBEEF"   (test-parse-from :strchar "\\ubeef"))))
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

