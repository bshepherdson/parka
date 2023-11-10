(ns parka.expr-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(def math-rules
  {:start    (p/value [:expr p/eof] first)
   :expr     (p/alt (p/action
                      [:mult    "+" :expr]
                      (fn [[m _ e]]
                        (+ m e)))
                    :mult)
   :mult     (p/alt (p/action
                      [:primary "*" :mult]
                      (fn [[p _ m :as input]]
                        (* p m)))
                    :primary)
   :primary  (p/alt (p/value [\( :expr \)] second)
                    :number)
   :number   (p/action
               [(p/? \-) :decimal]
               (fn [[minus decimal]]
                 (if minus
                   (- decimal)
                   decimal)))
   :decimal  (p/action (p/+ :digit)
                       #(Integer/parseInt (string/join %)))
   :digit    (p/one-of "0123456789")})

(defn test-parse [s]
  (p/parse-dynamic (p/grammar math-rules :start) "<test>" s))

(comment
  (p/compile (p/grammar math-rules :start))
  (test-parse "-17")
  (test-parse "0d")
  *e
  (p/parse-dynamic (p/grammar math-rules :start)
                   "<test>"
                   "17")
  )

(deftest math-test
  (testing "just numbers"
    (is (= {:success 1   :tail ""} (test-parse "1")))
    (is (= {:success 141 :tail ""} (test-parse "141")))
    (is (= {:success 0   :tail ""} (test-parse "000")))
    (is (= {:success -17 :tail ""} (test-parse "-17")))
    (is (= {:error {}}
           (test-parse "0d"))))
  ;; START HERE: Figure out why the error case for (test-parse "0d") is busted.
  ;; TODO:Consider building a fancy datafiable debugging scheme here, that uses
  ;; run-parser to keep track of the tree of parses and that writes a chain of
  ;; parsing states and results into an atom for perusing step by step.
  ;; Super powerful for debugging both the engine and a grammar that's giving
  ;; trouble. If I can get that working nicely, it can be a contribution to
  ;; users of parka as well as useful for me!
  (testing "factors"
    (is (= {:success 36  :tail ""} (test-parse "4*9")))
    (is (= {:success 396 :tail ""} (test-parse "4*99")))
    (is (= {:success 252 :tail ""} (test-parse "4*9*7"))))
  (testing "adding"
    (is (= {:success 13  :tail ""} (test-parse "4+9")))
    (is (= {:success 40  :tail ""} (test-parse "4*9+4")))
    (is (= {:success 67  :tail ""} (test-parse "4+9*7"))))
  (testing "parens"
    (is (= {:success 52  :tail ""} (test-parse "4*(9+4)")))
    (is (= {:success 91  :tail ""} (test-parse "(4+9)*7")))))

