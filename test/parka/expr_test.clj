(ns parka.expr-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [parka.api :as p]))

(def math-rules
  {:start    (p/pick [0] [:expr p/eof])
   :expr     (p/alt (p/action
                     [:mult    "+" :expr]
                     (fn [[m _ e]]
                       (+ m e)))
                    :mult)
   :mult     (p/alt (p/action
                     [:primary "*" :mult]
                     (fn [[p _ m]]
                       (* p m)))
                    :primary)
   :primary  (p/alt (p/pick [1] [\( :expr \)])
                    :number)
   :number   (p/action
              [(p/? \-) :decimal]
              (fn [[minus decimal]]
                (if minus
                  (- decimal)
                  decimal)))
   :decimal  (p/action (p/+ :digit)
                       #(Integer/parseInt (string/join %)))
   :digit    (p/span \0 \9)})

(defn test-parse [s]
  (p/parse (p/compile (p/grammar math-rules :start)) "<test>" s))

(deftest math-test
  (testing "just numbers"
    (is (= {:success 1}   (test-parse "1")))
    (is (= {:success 141} (test-parse "141")))
    (is (= {:success 0}   (test-parse "000")))
    (is (= {:success -17} (test-parse "-17")))
    (is (= {:error :parka.machine.peg/expected-failure}
           (test-parse "0d"))))
  (testing "factors"
    (is (= {:success 36}   (test-parse "4*9")))
    (is (= {:success 396}  (test-parse "4*99")))
    (is (= {:success 252}  (test-parse "4*9*7"))))
  (testing "adding"
    (is (= {:success 13} (test-parse "4+9")))
    (is (= {:success 40} (test-parse "4*9+4")))
    (is (= {:success 67} (test-parse "4+9*7"))))
  (testing "parens"
    (is (= {:success 52} (test-parse "4*(9+4)")))
    (is (= {:success 91} (test-parse "(4+9)*7")))))

