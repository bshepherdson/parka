(ns parka.machine.compiler
  "Handles the compilation from the PEG grammar syntax to the parsing machine
  instructions."
  (:refer-clojure :exclude [compile])
  (:require
    [parka.errors :as errs]))

(defmulti compile
  (fn [pat]
    (cond
      (map?     pat) (:parka/type pat)
      (keyword? pat) :parka/nonterminal
      (char?    pat) :parka/char
      (string?  pat) :parka/string
      (vector?  pat) :parka/seq
      (set?     pat) :parka/set
      :else (throw (ex-info "bad compile pattern" {:value pat})))))

(defn compile-expr [p]
  (into (compile p) [[:end]]))

(defmethod compile :parka/char
  [p]
  [[:char p]])

(defmethod compile :parka/string
  [s]
  (mapv #(vector :char %) s))

(defmethod compile :parka/any
  [_]
  [[:any]])

(defmethod compile :parka/seq
  [ps]
  (into [] (mapcat compile ps)))

(defn- compile-alt [alts]
  (condp = (count alts)
    0 []
    1 (compile (first alts))
    (let [rhs (compile-alt (rest  alts))
          lhs (compile     (first alts))]
      (into [] (concat [[:choice (+ 2 (count lhs))]]
                       lhs
                       [[:commit (inc (count rhs))]]
                       rhs)))))

(defmethod compile :parka/alt
  [{:parka/keys [alts]}]
  (compile-alt alts))

(defmethod compile :parka/not
  [{:parka/keys [inner]}]
  (let [p (compile inner)]
    (into []
          (concat [[:choice (+ 2 (count p))]]
                  p
                  [[:fail-twice]]))))

(defmethod compile :parka/and
  [{:parka/keys [inner]}]
  (let [p (compile inner)]
    (into []
          (concat [[:choice (+ 2 (count p))]]
                  p
                  [[:back-commit 2]
                   [:fail]]))))

(defmethod compile :parka/set
  [chs]
  [[:charset chs]])

(defmethod compile :parka/nonterminal
  [nonterminal]
  [[:open-call nonterminal]])

(defn compile-grammar [[[sym pat] & rs] labels code]
  (let [compiled (compile pat)
        labels'  (assoc labels sym (count code))
        code'    (into code (concat compiled [[:return]]))]
    (if (empty? rs)
      [labels' code']
      (recur rs labels' code'))))

(defn resolve-call [[op v :as code] pc labels]
  (if (#{:open-call :open-jump} op)
    (if-let [target (get labels v)]
      [(if (= :open-call op) :call :jump) (- target pc)]
      (throw (ex-info (str "unknown grammar rule: " v) {:rule v})))
    ; Nothing to do for non-calls.
    code))

(defn resolve-calls [code labels]
  (into [] (map-indexed #(resolve-call %2 %1 labels) code)))

(defmethod compile :parka/grammar
  [{:parka/keys [rules start]}]
  (let [[labels code] (compile-grammar rules
                                       {:parka/top 0}
                                       [[:open-call start]
                                        [:open-jump :parka/end]])
        labels'       (assoc labels :parka/end (count code))
        ;code'         (into code [[:end]])
        ]
    (resolve-calls code labels')))

(defmethod compile :parka/star
  [{:parka/keys [inner]}]
  (let [p (compile inner)]
    (into []
          (concat [[:choice (+ 2 (count p))]]
                  p
                  [[:partial-commit (- (count p))]]))))

