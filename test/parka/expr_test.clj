(ns parka.expr-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(def math-rules
  {:start    [:expr p/eof]
   :expr     (p/alt [:mult    "+" :expr] :mult)
   :mult     (p/alt [:primary "*" :mult] :primary)
   :primary  (p/alt [\( :expr \)] (p/+ :decimal))
   :decimal  (p/one-of "0123456789")})

(def math (p/compile (p/grammar math-rules :start)))

(defn test-parse [s]
  (p/parse math "<test>" s))

(deftest math-test
  (testing "just numbers"
    (is (= {:success "1"}   (test-parse "1")))
    (is (= {:success "141"} (test-parse "141")))
    (is (= {:success "000"} (test-parse "000")))
    (is (= {:error :parka.machine.peg/expected-failure}
           (test-parse "0d"))))
  (testing "factors"
    (is (= {:success "4*9"}   (test-parse "4*9")))
    (is (= {:success "4*99"}  (test-parse "4*99")))
    (is (= {:success "4*9*7"} (test-parse "4*9*7"))))
  (testing "adding"
    (is (= {:success "4+9"}   (test-parse "4+9")))
    (is (= {:success "4*9+4"} (test-parse "4*9+4")))
    (is (= {:success "4+9*7"} (test-parse "4+9*7"))))
  (testing "parens"
    (is (= {:success "4*(9+4)"} (test-parse "4*(9+4)")))
    (is (= {:success "(4+9)*7"} (test-parse "(4+9)*7")))))

