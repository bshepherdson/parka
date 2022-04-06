(ns parka.expr-test
  (:require
    [clojure.string :as string]
    [clojure.test :refer [deftest is testing]]
    [parka.api :as p]))

(def math-rules
  {:start    (p/action [:expr p/eof] :expr)
   :expr     (p/action
               (p/alt [:mult    "+" :expr] [:mult])
               (fn [{:keys [mult expr]}]
                 (if expr
                   (+ mult expr)
                   mult)))
   :mult     (p/action
               (p/alt [:primary "*" :mult] [:primary])
               (fn [{:keys [primary mult]}]
                 (if mult
                   (* mult primary)
                   primary)))
   :primary  (p/action
               (p/alt [\( :expr \)]
                      :number)
               (fn [res]
                 (or (:expr res) res)))
   :number   (p/action
               [{:minus (p/? \-)} :decimal]
               (fn [{:keys [minus decimal]}]
                 (if minus
                   (- decimal)
                   decimal)))
   :decimal  (p/action (p/+ :digit)
                       #(Integer/parseInt (string/join %)))
   :digit    (p/one-of "0123456789")})

(def math (p/compile (p/grammar math-rules :start)))

(defn test-parse [s]
  (p/parse math "<test>" s))

(deftest math-test
  (testing "just numbers"
    (is (= {:success 1}   (test-parse "1")))
    (is (= {:success 141} (test-parse "141")))
    (is (= {:success 0}   (test-parse "000")))
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

