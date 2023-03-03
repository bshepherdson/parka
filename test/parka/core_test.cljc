(ns parka.core-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [parka.api :as sut]
   [parka.test-util]))

(defn- test-parse [p input]
  (sut/parse (sut/compile p) "<test>" input))

(defn- error
  [l c msg]
  {:error {:parka/parse-error true
           :parka/loc         (str "<test> line " l " col " c)
           :parka/message     msg}})

(defn- expected [l c exps]
  (assoc-in (error l c "failed expectation") [:error :parka/expectations] exps))

(deftest test-lit
  (is (= "foo" (test-parse "foo" "foo")))
  (is (ex-info? "Expected \"foo\", got \"for\""
                {:expected "foo" :got "for"}
                (test-parse "foo" "for"))))

(deftest test-sets
  (is (= "f" (test-parse #{\f} "f")))
  (is (= "f" (test-parse #{\f} "fo")))
  (is (ex-info? "Expected one of #{\\a \\b \\c}, got f"
                {:expected #{\a \b \c} :got \f}
                (test-parse #{\a \b \c} "f")))
  (is (ex-info? "Unexpected EOF! Expected one of #{\\a \\b \\c}"
                {:expected #{\a \b \c}}
                (test-parse #{\a \b \c} ""))))

(deftest test-star
  (let [p (sut/* \a)]
    (is (= ["a" "a" "a" "a"] (test-parse p "aaaab_c")))
    (is (= ["a"]             (test-parse p "ab_cX")))
    (is (= []                (test-parse p "X")))))

(deftest test-plus
  (let [p (sut/+ \a)]
    (is (= ["a" "a" "a" "a"] (test-parse (sut/+ \a) "aaaab_c")))
    (is (= ["a"]             (test-parse p "ab_cX")))
    (is (ex-info? "Expected \\a, got \\X"
                  {:expected \a :got \X}
                  (test-parse p "X")))))

(deftest test-alt
  (testing "basics"
    (let [p (sut/alt "foo" "bar" "baz")]
      (is (= "foo" (test-parse p "foo")))
      (is (= "bar" (test-parse p "bar")))
      (is (= "baz" (test-parse p "baz")))
      (is (ex-info? "Expected one of: \"foo\", \"bar\", \"baz\""
                    {:expected (list "\"foo\"" "\"bar\"" "\"baz\"")}
                    (test-parse p "asdf")))))

  (testing "backtracking sequences"
    (let [p (sut/alt
             ["0x" (sut/* (set "0123456789abcdefABCDEF"))]
             (sut/* (set "0123456789")))]
      (is (= ["1" "2"]
             (test-parse p "12")))
      (is (= ["0x" ["1" "2" "a" "B" "3"]]
             (test-parse p "0x12aB3"))))))

(deftest test-and
  (let [p ["a" (sut/and "bbb") (sut/* "b")]]
    (is (= ["a" nil ["b" "b" "b"]]
           (test-parse p "abbb")))
    (is (= ["a" nil ["b" "b" "b" "b" "b"]]
           (test-parse p "abbbbb")))
    (is (ex-info? "Unexpected EOF! Expected \"bbb\""
                  {:expected "bbb"}
                  (test-parse p "abb")))))

(deftest test-not
  (let [p ["a" (sut/not "b") sut/any]]
    (is (= ["a" nil "c"]
           (test-parse p "acd")))
    (is (ex-info? "Expected not to be followed by \"b\""
                  {:expected {:not "\"b\""}}
                  (test-parse p "abd")))))

(deftest test-?
  (let [p [(sut/? "a") "b"]]
    (is (= ["a" "b"] (test-parse p "ab")))
    (is (= [nil "b"] (test-parse p "b")))
    (is (ex-info? "Expected \"b\", got \"c\""
                  {:expected "b" :got "c"}
                  (test-parse p "ac")))
    (is (ex-info? "Expected \"b\", got \"c\""
                  {:expected "b" :got "c"}
                  (test-parse p "c")))
    (is (ex-info? "Expected \"b\", got \"c\""
                  {:expected "b" :got "c"}
                  (test-parse p "cb")))))

(deftest test-eof
  (let [base ["a" (sut/* \b)]
        eofd (conj base sut/eof)]
    (is (= ["a" ["b" "b" "b"]]
           (test-parse base "abbbc")))
    (is (= ["a" ["b" "b" "b"] nil]
           (test-parse eofd "abbb")))
    (is (ex-info? "Expected EOF, but got c"
                  {:expected {:not "anything"}}
                  (test-parse eofd "abbbc")))))

(deftest test-span
  (let [p (sut/span \a \z)]
    (is (= "d" (test-parse p "d")))
    (is (= "a" (test-parse p "a")))
    (is (= "z" (test-parse p "z")))
    (let [alpha [\a \b \c \d \e \f \g \h \i \j \k \l \m
                 \n \o \p \q \r \s \t \u \v \w \x \y \z]]
      (is (ex-info? (str "Expected one of #{"
                         (str/join " " (map pr-str alpha))
                         "}, got A")
                    {:expected (set alpha) :got \A}
                    (test-parse p "A"))))))

#_(deftest test-lit-ic
    (is (= "foo" (test-parse (sut/lit-ic "foo") "foo")))
    (is (= "foo" (test-parse (sut/lit-ic "foo") "FOO")))
    (is (thrown-with-msg? Exception #"expected literal 'foo'"
                          (test-parse (sut/lit "foo") "for"))))

#_(deftest case-insensitive-test
    (testing "single characters"
      (let [p (sut/ic \a)]
        (is (= {:success "a"} (test-parse p "a")))
        (is (= {:success "A"} (test-parse p "A")))
        (is (= {:error #:parka{:parse-error true
                               :loc "<test> line 1 col 0"
                               :message "failed expectation",
                               :expectations #{\A \a}}}
               (test-parse p "D")))
        (is (= {:success ["a" "a" "A" "A" "a"]}
               (test-parse (sut/* p) "aaAAa")))))

    (testing "set of characters"
      (let [p (sut/ic (set "abc"))]
        (is (= {:success "a"} (test-parse p "a")))
        (is (= {:success "A"} (test-parse p "A")))
        (is (= {:success "b"} (test-parse p "b")))
        (is (= {:success "B"} (test-parse p "B")))
        (is (= {:success "c"} (test-parse p "c")))
        (is (= {:success "C"} (test-parse p "C")))
        (is (= {:error #:parka{:parse-error true
                               :loc "<test> line 1 col 0"
                               :message "failed expectation",
                               :expectations #{\A \a \B \b \C \c}}}
               (test-parse p "D")))
        (is (= {:success ["b" "a" "C" "A" "b"]}
               (test-parse (sut/* p) "baCAb")))))

    (testing "string literals"
      (let [p (sut/ic "abc")]
        (is (= {:success "abc"} (test-parse p "abc")))
        (is (= {:success "ABC"} (test-parse p "ABC")))
        (is (= {:success "aBc"} (test-parse p "aBc")))
        (is (= {:success "ABc"} (test-parse p "ABc")))
        (is (= {:success "AbC"} (test-parse p "AbC")))
        (is (= {:success "abC"} (test-parse p "abC")))
        (is (= {:error #:parka{:parse-error true
                               :loc "<test> line 1 col 2"
                               :message "failed expectation"
                               :expectations #{\C \c}}}
               (test-parse p "AbD")))
        (is (= {:success ["abC" "ABC" "abc" "Abc"]}
               (test-parse (sut/* p) "abCABCabcAbc")))
        (is (= {:success []} (test-parse (sut/* p) "")))))

    (testing "alts"
      (let [p (sut/ic [(sut/alt "true"
                                "false"
                                (sut/str (sut/* (sut/span \a \f))))
                       sut/eof])]
        (is (= {:success ["TRue"  nil]} (test-parse p "TRue")))
        (is (= {:success ["faLSe" nil]} (test-parse p "faLSe")))
        (is (= {:success [""      nil]} (test-parse p "")))
        (is (= {:success ["ABc"   nil]} (test-parse p "ABc")))
        (is (= {:success ["FFe"   nil]} (test-parse p "FFe")))
        #_(is (= {:error #:parka{:parse-error true
                                 :loc "<test> line 1 col 4"
                                 :message "failed expectation"
                                 :expectations #{\E \e}}}
                 (test-parse p "falS")))
        #_(is (= {:success ["abC" "ABC" "abc" "Abc"]}
                 (test-parse (sut/* p) "abCABCabcAbc")))
        #_(is (= {:success []} (test-parse (sut/* p) ""))))))
