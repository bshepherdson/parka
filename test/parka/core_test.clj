(ns parka.core-test
  (:require
    [clojure.test :refer [are deftest is testing]]
    [parka.core :as sut]
    [parka.streams :as s]))

(defn- test-parse [p input]
  (sut/parse-str p "<test>" input))

(defn- test-partial [p input]
  (select-keys (sut/parse p (s/stream "<test>" input))
               [:pos :value]))

(defn- test-fail [p input]
  (try
    (test-parse p input)
    (throw (Exception. "parser was expected to fail, but it succeeded"))
    (catch Exception e e)))

(deftest test-lit
  (is (= "foo" (test-parse (sut/lit "foo") "foo")))
  (is (thrown-with-msg? Exception #"expected literal 'foo'"
                        (test-parse (sut/lit "foo") "for"))))

(deftest test-lit-ic
  (is (= "foo" (test-parse (sut/lit-ic "foo") "foo")))
  (is (= "foo" (test-parse (sut/lit-ic "foo") "FOO")))
  (is (thrown-with-msg? Exception #"expected literal 'foo'"
                        (test-parse (sut/lit "foo") "for"))))

(deftest test-alt
  (testing "basics"
    (let [p (sut/alt (sut/lit    "foo")
                     (sut/lit    "bar")
                     (sut/lit-ic "baz"))]
      (is (= "foo" (test-parse p "foo")))
      (is (= "bar" (test-parse p "bar")))
      (is (= "baz" (test-parse p "baz")))
      (is (= "baz" (test-parse p "BaZ")))
      (is (= "<test> line 1 col 0: expected literal 'foo' or literal 'bar' or literal 'baz'"
           (try (test-parse p "asdf")
                "wrong success!"
                (catch Exception e
                  (.getMessage e)))))))

  (testing "backtracking sequences"
    (let [p (sut/alt
              (sut/pseq "0x" (sut/stringify (sut/many1 (sut/span \0 \9))))
              (sut/stringify (sut/many1 (sut/span \0 \9))))]
      (is (= "12" (test-parse p "12")))
      (is (= ["0x" "123"] (test-parse p "0x123"))))))

(deftest test-pseq
  (testing "direct calls"
    (let [p (sut/pseq (sut/lit "a") (sut/lit "b") (sut/lit-ic "C"))]
      (is (= ["a" "b" "C"] (test-parse p "abc")))
      (is (= ["a" "b" "C"] (test-parse p "abC")))
      (is (thrown-with-msg? Exception #"expected literal 'a'"
                            (test-parse p "xbC")))
      (is (thrown-with-msg? Exception #"expected literal 'b'"
                            (test-parse p "ayC")))
      (is (thrown-with-msg? Exception #"expected literal 'C'"
                            (test-parse p "abZ")))))

  (testing "vector shorthand"
    (let [p [(sut/lit "a") (sut/lit "b") (sut/lit-ic "C")]]
      (is (= ["a" "b" "C"] (test-parse p "abc")))
      (is (= ["a" "b" "C"] (test-parse p "abC")))
      (is (thrown-with-msg? Exception #"expected literal 'a'"
                            (test-parse p "xbC")))
      (is (thrown-with-msg? Exception #"expected literal 'b'"
                            (test-parse p "ayC")))
      (is (thrown-with-msg? Exception #"expected literal 'C'"
                            (test-parse p "abZ"))))))

(deftest test-pseq-at
  (let [mkp #(sut/pseq-at % (sut/lit "a") (sut/lit "b") (sut/lit-ic "C"))]
    (is (= "a" (test-parse (mkp 0) "abc")))
    (is (= "b" (test-parse (mkp 1) "abc")))
    (is (= "C" (test-parse (mkp 2) "abc")))
    (are [i lit input]
         (thrown-with-msg? Exception lit
                           (test-parse (mkp i) input))
         0 #"expected literal 'a'" "xbC"
         0 #"expected literal 'b'" "ayC"
         0 #"expected literal 'C'" "abz"
         1 #"expected literal 'a'" "xbC"
         1 #"expected literal 'b'" "ayC"
         1 #"expected literal 'C'" "abz"
         2 #"expected literal 'a'" "xbC"
         2 #"expected literal 'b'" "ayC"
         2 #"expected literal 'C'" "abz")))

(deftest test-optional
  (let [p (sut/pseq (sut/optional (sut/lit "a"))
                    (sut/lit "b"))]
    (is (= ["a" "b"] (test-parse p "ab")))
    (is (= [nil "b"] (test-parse p "b"))))
  (let [p (sut/pseq (sut/opt (sut/lit "a"))
                    (sut/lit "b"))]
    (is (= ["a" "b"] (test-parse p "ab")))
    (is (= [nil "b"] (test-parse p "b")))))

(deftest test-one-of
  (let [p (sut/one-of "abc")]
    (is (= \a (test-parse p "a")))
    (is (= \b (test-parse p "b")))
    (is (= \c (test-parse p "c")))
    (is (thrown-with-msg? Exception #"unexpected EOF"
                          (test-parse p "")))
    (is (thrown-with-msg? Exception #"expected one of: abc"
                          (test-parse p "d")))))

(deftest test-none-of
  (let [p (sut/none-of "abc")]
    (is (= \d (test-parse p "d")))
    (is (thrown-with-msg? Exception #"unexpected EOF"
                          (test-parse p "")))
    (is (thrown-with-msg?
          Exception #"found illegal a, expected character other than abc"
          (test-parse p "a")))
    (is (thrown-with-msg?
          Exception #"found illegal b, expected character other than abc"
          (test-parse p "b")))
    (is (thrown-with-msg?
          Exception #"found illegal c, expected character other than abc"
          (test-parse p "c")))))

(deftest test-span
  (let [p (sut/span \a \z)]
    (is (= \d (test-parse p "d")))
    (is (= \a (test-parse p "a")))
    (is (= \z (test-parse p "z")))
    (is (thrown-with-msg?
          Exception #"expected character in range a to z; found A"
          (test-parse p "A")))))

(deftest test-many
  (let [p (sut/many (sut/alt (sut/lit "_")
                             (sut/span \a \z)))]
    (is (= [\a \b "_" \c] (test-parse p "ab_c")))
    (is (= {:value [] :pos 0}
           (test-partial p "X")))
    (is (= {:value [\a \b "_" \c] :pos 4}
           (test-partial p "ab_cX")))))

(deftest test-many-min
  (let [p (sut/many-min 3 (sut/alt (sut/lit "_")
                                   (sut/span \a \z)))]
    (is (= [\a \b "_" \c] (test-parse p "ab_c")))
    (is (thrown-with-msg? Exception #"minimum 3" (test-parse p "ab")))))

(deftest test-many-drop
  (is (= nil (test-parse (sut/many-drop (sut/one-of " \t\r\n"))
                         "   \t\t  \n\t\t  ")))
  (is (= ["a" nil "b"]
         (test-parse (sut/pseq
                       (sut/lit "a")
                       (sut/many-drop (sut/one-of " \t\r\n"))
                       (sut/lit "b"))
                     "a   \t\t  \n\t\t  b"))))

(deftest test-sep-by
  (let [p (sut/sep-by (sut/lit "a") (sut/lit "b"))]
    (is (= [] (test-parse p "")))
    (is (= ["a"] (test-parse p "a")))
    (is (= ["a" "a"] (test-parse p "aba")))
    (is (= ["a" "a" "a" "a"] (test-parse p "abababa")))
    (is (= {:pos 3 :value ["a" "a"]}
           (test-partial p "abab")))
    (is (= {:pos 0 :value []}
           (test-partial p "bb")))))

(deftest test-sep-by1
  (let [p (sut/sep-by1 (sut/lit "a") (sut/lit "b"))]
    (is (thrown-with-msg? Exception #"expected at least 1:"
                          (test-parse p "")))
    (is (= ["a"] (test-parse p "a")))
    (is (= ["a" "a"] (test-parse p "aba")))
    (is (= ["a" "a" "a" "a"] (test-parse p "abababa")))
    (is (= {:pos 3 :value ["a" "a"]}
           (test-partial p "abab")))))

(deftest test-many-till
  (testing "basics"
    (let [p (sut/pseq-at
              1 (sut/lit "\"")
              (sut/stringify (sut/many-till sut/any (sut/lit "\""))))]
      (is (= "abc" (test-parse p "\"abc\"")))
      (is (= ""    (test-parse p "\"\"")))
      (testing "unterminated string"
        (is (thrown-with-msg? Exception #"expected literal '\"'"
                              (test-parse p "\"ab"))))))
  (testing "bad inner"
    (let [p (sut/between (sut/lit "a") (sut/lit "c")
                         (sut/many (sut/lit "b")))]
      (is (= ["b" "b" "b"] (test-parse p "abbbc")))
      (is (thrown-with-msg? Exception #"expected literal 'c'"
          (test-parse p "abbdc"))))))

(deftest test-string-autoupgrade
  (is (= "a" (test-parse "a" "a")))
  (is (= ["a" "b" "c"] (test-parse (sut/pseq "a" (sut/lit "b") "c") "abc"))))

(deftest test-symbol-autoupgrade
  (let [g (sut/grammar {:start (sut/pseq "a" :b (sut/sym :c))
                        :b     "b"
                        :c     (sut/lit-ic "c")})]
    (is (= ["a" "b" "c"] (sut/parse-str g "<test>" "abc")))))

(deftest test-lookahead
  (let [g (sut/grammar {:start (sut/pseq "a" (sut/lookahead "bbb") (sut/many "b"))})]
    (is (= ["a" nil ["b" "b" "b"]] (sut/parse-str g "<test>" "abbb")))
    (is (= ["a" nil ["b" "b" "b" "b" "b"]] (sut/parse-str g "<test>" "abbbbb")))
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo #"expected literal 'bbb'"
          (sut/parse-str g "<test>" "abb")))))

(deftest test-not
  (let [g (sut/grammar {:start (sut/pseq "a" (sut/not "b") sut/any)})]
    (is (= ["a" nil \c] (sut/parse-str g "<test>" "ac")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/parse-str g "<test>" "abc")))))

