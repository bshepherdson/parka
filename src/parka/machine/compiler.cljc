(ns parka.machine.compiler
  "Handles the compilation from the PEG grammar syntax to the parsing machine
  instructions."
  (:refer-clojure :exclude [compile])
  (:require
   [parka.ast :as ast]
   [parka.errors :as errs]))

(defmulti compile*
  (fn [pat _]
    (:parka/type pat)))

(defn compile [p s]
  (compile* (ast/->parser p) s))

(defn compile-expr [p]
  (into (compile p {:capture? true}) [[:end]]))

(defmethod compile* :parka/char
  [{ch :parka/char} {:keys [capture?]}]
  (if capture?
    [[:char ch] [:push (str ch)]]
    [[:char ch]]))

(defmethod compile* :parka/string
  [{target :parka/string} {:keys [capture?]}]
  (let [core (mapv #(vector :char %) target)]
    (if capture?
      (into [] (concat [[:mark]] core [[:capture]]))
      core)))

(defmethod compile* :parka/any
  [_ {:keys [capture?]}]
  (if capture?
    [[:mark] [:any] [:capture]]
    [[:any]]))

(defmethod compile* :parka/seq
  [{ps :parka/seq} {:keys [nested? capture?] :as s}]
  ;; If nested, don't push a new value.
  (let [pre (when (and capture? (not nested?))
              [[:push []]])
        s'  (dissoc s :nested?)
        compiled (mapcat (if capture?
                           #(into (compile % s') [[:apply-capture-2 conj]])
                           #(compile % s'))
                         ps)]
    (into [] (concat pre compiled))))

(defn- compile-alt [alts s]
  (condp = (count alts)
    0 []
    1 (compile (first alts) s)
    (let [rhs (compile-alt (rest  alts) s)
          lhs (compile     (first alts) s)]
      (into [] (concat [[:choice (+ 2 (count lhs))]]
                       lhs
                       [[:commit (inc (count rhs))]]
                       rhs)))))

(defmethod compile* :parka/alt
  [{:parka/keys [alts]} s]
  (compile-alt alts s))

(defmethod compile* :parka/not
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  ; No need to really capture lookahead.
  (let [p (compile inner (assoc s :capture? false))]
    (into []
          (concat [[:choice (+ 2 #_(if capture? 3 0) (count p))]]
                  p
                  [[:fail-twice]]
                  (when capture? [[:push nil]])))))

(defmethod compile* :parka/and
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  ; No need to really capture lookahead.
  (let [p (compile inner (assoc s :capture? false))]
    (into []
          (concat [[:choice (+ 2 #_(if capture? 1 0) (count p))]]
                  p
                  ;(when capture? [[:push nil]]) ; But need a dummy if capturing.
                  [[:back-commit 2]
                   [:fail]]))))

(defmethod compile* :parka/set
  [{chs :parka/set} {:keys [capture?]}]
  (into [] (concat (when capture? [[:mark]])
                   [[:charset chs]]
                   (when capture? [[:capture]]))))

(defmethod compile* :parka/nonterminal
  [{:parka/keys [nonterminal]} _]
  [[:open-call nonterminal]])

(defn compile-grammar [[[sym pat] & rs] labels code s]
  (let [compiled (compile pat s)
        labels'  (assoc labels sym (count code))
        code'    (into code (concat compiled [[:return]]))]
    (if (empty? rs)
      [labels' code']
      (recur rs labels' code' s))))

(defn resolve-call [[op v :as code] pc labels]
  (if (#{:open-call :open-jump} op)
    (if-let [target (get labels v)]
      [(if (= :open-call op) :call :jump) (- target pc)]
      (throw (ex-info (str "unknown grammar rule: " v) {:rule v})))
    ; Nothing to do for non-calls.
    code))

(defn resolve-calls [code labels]
  (into [] (map-indexed #(resolve-call %2 %1 labels) code)))

(defmethod compile* :parka/grammar
  [{:parka/keys [rules start]} s]
  (let [[labels code] (compile-grammar rules
                                       {:parka/top 0}
                                       [[:open-call start]
                                        [:open-jump :parka/end]]
                                       (assoc s :capture? true))
        labels'       (assoc labels :parka/end (count code))]
    (resolve-calls code labels')))

(defmethod compile* :parka/star
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  (let [p (compile inner s)
        cap-width (if capture? 1 0)]
    (into []
          (concat (when capture? [[:push []]])
                  [[:choice (+ 2 cap-width (count p))]]
                  p
                  (when capture? [[:apply-capture-2 conj]])
                  [[:partial-commit (- (+ cap-width (count p)))]]))))

(defmethod compile* :parka/action
  [{:parka/keys [inner action]} {:keys [capture?] :as s}]
  (into [] (concat (compile inner s)
                   (when capture? [[:apply-capture-1 action]]))))
