(ns parka.expr-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(defn- expected [line col exps]
  {:error #:parka{:parse-error true
                  :loc (str "<test> line " line " col " col)
                  :expectations ["EOF"]}})

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

(deftest math-test
  (testing "just numbers"
    (is (= {:success 1}   (test-parse "1")))
    (is (= {:success 141} (test-parse "141")))
    (is (= {:success 0}   (test-parse "000")))
    (is (= {:success -17} (test-parse "-17")))
    (is (= (expected 1 1 ["EOF"])
           (test-parse "0d"))))
  (testing "factors"
    (is (= {:success 36}  (test-parse "4*9")))
    (is (= {:success 396} (test-parse "4*99")))
    (is (= {:success 252} (test-parse "4*9*7"))))
  (testing "adding"
    (is (= {:success 13} (test-parse "4+9")))
    (is (= {:success 40} (test-parse "4*9+4")))
    (is (= {:success 67} (test-parse "4+9*7"))))
  (testing "parens"
    (is (= {:success 52} (test-parse "4*(9+4)")))
    (is (= {:success 91} (test-parse "(4+9)*7")))))
