(ns parka.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [parka.api :as sut]))

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
  (is (= {:success "foo"} (test-parse "foo" "foo")))
  (is (= (expected 1 2 ["o"])
         (test-parse "foo" "for"))))

#_(deftest test-lit-ic
    (is (= "foo" (test-parse (sut/lit-ic "foo") "foo")))
    (is (= "foo" (test-parse (sut/lit-ic "foo") "FOO")))
    (is (thrown-with-msg? Exception #"expected literal 'foo'"
                          (test-parse (sut/lit "foo") "for"))))

(deftest test-sets
  (is (= {:success "f"} (test-parse #{\f} "f")))
  (is (= {:success "f"} (test-parse #{\f} "fo")))
  (is (= (expected 1 0 (into #{} "abc"))
         (test-parse #{\a \b \c} "f"))))

(deftest test-one-of
  (let [p (set "abc")]
    (is (= {:success "a"} (test-parse p "a")))
    (is (= {:success "b"} (test-parse p "b")))
    (is (= {:success "c"} (test-parse p "c")))
    (is (= (expected 1 0 #{\a \b \c})
           (test-parse p "")))
    (is (= (expected 1 0 #{\a \b \c})
           (test-parse p "d")))))

(deftest test-star
  (let [p (sut/* \a)]
    (is (= {:success ["a" "a" "a" "a"]} (test-parse p "aaaab_c")))
    (is (= {:success ["a"]}             (test-parse p "ab_cX")))
    (is (= {:success []}                (test-parse p "X")))))

(deftest test-plus
  (let [p (sut/+ \a)]
    (is (= {:success ["a" "a" "a" "a"]} (test-parse (sut/+ \a) "aaaab_c")))
    (is (= {:success ["a"]}             (test-parse p "ab_cX")))
    (is (= (expected 1 0 ["a"])         (test-parse p "X")))))

(deftest test-alt
  (testing "basics"
    (let [p (sut/alt "foo" "bar" "baz")]
      (is (= {:success "foo"} (test-parse p "foo")))
      (is (= {:success "bar"} (test-parse p "bar")))
      (is (= {:success "baz"} (test-parse p "baz")))
      (is (= (expected 1 0 ["b"])
             (test-parse p "asdf")))))

  (testing "backtracking sequences"
    (let [p (sut/alt
             ["0x" (sut/* (set "0123456789abcdefABCDEF"))]
             (sut/* (set "0123456789")))]
      (is (= {:success ["1" "2"]}
             (test-parse p "12")))
      (is (= {:success ["0x" ["1" "2" "a" "B" "3"]]}
             (test-parse p "0x12aB3"))))))

(deftest test-and
  (let [p ["a" (sut/and "bbb") (sut/* "b")]]
    (is (= {:success ["a" nil ["b" "b" "b"]]}
           (test-parse p "abbb")))
    (is (= {:success ["a" nil ["b" "b" "b" "b" "b"]]}
           (test-parse p "abbbbb")))
    (is (= {:error :parka.machine.peg/expected-failure}
           (test-parse p "abb")))))

(deftest test-not
  (let [p ["a" (sut/not "b") sut/any]]
    (is (= {:success ["a" nil "c"]}
           (test-parse p "acd")))
    (is (= {:error :parka.machine.peg/expected-failure}
           (test-parse p "abd")))))

(deftest test-?
  (let [p [(sut/? "a") "b"]]
    (is (= {:success ["a" "b"]} (test-parse p "ab")))
    (is (= {:success [nil "b"]} (test-parse p "b")))))

(deftest test-eof
  (let [base ["a" (sut/* \b)]
        eofd (conj base sut/eof)]
    (is (= {:success ["a" ["b" "b" "b"]]}
           (test-parse base "abbbc")))
    (is (= {:success ["a" ["b" "b" "b"] nil]}
           (test-parse eofd "abbb")))
    (is (= {:error :parka.machine.peg/expected-failure}
           (test-parse eofd "abbbc")))))

(deftest test-span
  (let [p (sut/span \a \z)]
    (is (= {:success "d"} (test-parse p "d")))
    (is (= {:success "a"} (test-parse p "a")))
    (is (= {:success "z"} (test-parse p "z")))
    (is {:error :parka.machine.peg/expected-failure}
        (test-parse p "A"))))

(deftest case-insensitive-test
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
      (is (= {:error #:parka{:parse-error true
                             :loc "<test> line 1 col 4"
                             :message "failed expectation"
                             :expectations #{\E \e}}}
             (test-parse p "falS")))
      #_(is (= {:success ["abC" "ABC" "abc" "Abc"]}
               (test-parse (sut/* p) "abCABCabcAbc")))
      #_(is (= {:success []} (test-parse (sut/* p) ""))))))

(comment
  {:error :parka.machine.peg/expected-failure})
