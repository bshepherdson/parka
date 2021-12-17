(ns parka.json-test
  "Test that builds and exercises a JSON parser using this library."
  (:require
    [clojure.test :refer [deftest is]]
    [parka.core :as p]))

(def json-rules
  {:start      (p/between :ws :json-value)
   :json-value (p/alt :array :object :null :bool :string :number)
   :ws         (p/many-drop (p/one-of " \t\r\n"))
   :null       (p/with-action "null" (constantly nil))
   :bool       (p/with-action
                 (p/alt "true" "false")
                 (fn [v _] (= v "true")))
   :string     (p/stringify (p/pseq-at 1 "\""
                                      (p/many-till p/any "\"")))
   :number     (p/with-action
                 (p/pseq (p/optional (p/one-of "+-"))
                         (p/stringify (p/many1 (p/span \0 \9))))
                 (fn [[sign s] _]
                   (let [n (Integer/parseInt s)]
                     (if (= \- sign)
                       (- n)
                       n))))
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
  (is (= 1   (test-parse-from :number "1")))
  (is (= 123 (test-parse-from :number "0123")))
  (is (= 0   (test-parse-from :number "0")))
  (is (= 88  (test-parse-from :number "+88")))
  (is (= -41 (test-parse-from :number "-41"))))

(deftest test-ws
  (is (= nil (test-parse-from :ws "   \t\t \n   \t"))))

(deftest test-comma
  (is (= [nil "," nil] (test-parse-from :comma "    \t  ,\n   "))))

(deftest test-string
  (is (= "asdf" (test-parse-from :string "\"asdf\"")))
  (is (= ""     (test-parse-from :string "\"\""))))

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

