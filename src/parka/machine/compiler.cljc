(ns parka.machine.compiler
  "Handles the compilation from the PEG grammar syntax to the parsing machine
  instructions."
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]))

(defn- parser-type [pat]
  (cond
    (map?        pat) (:parka/type pat)
    (keyword?    pat) :parka/nonterminal
    (char?       pat) :parka/char
    (string?     pat) :parka/string
    (set?        pat) :parka/set
    (sequential? pat) :parka/seq
    :else (throw (ex-info "bad compile pattern" {:value pat}))))

(defmulti compile (fn [p _] (parser-type p)))

(defn compile-expr [p]
  (into (compile p {:capture? true}) [[:end]]))

(defn- up-down [ch]
  [(first (str/upper-case ch))
   (first (str/lower-case ch))])

(defn- case-char
  "Given a character and the options, checks if `decase?` is true, and if case
  matters for this character.
  If it does, returns `[:charset #{lower upper}]`.
  If it doesn't, returns `[:char ch]`."
  [ch {:keys [decase?]}]
  (if-let [[uc lc] (when decase? (up-down ch))]
    [:charset #{uc lc}]
    [:char ch]))

(defmethod compile :parka/char
  [p {:keys [capture?] :as s}]
  ;; Four cases here, for all four settings of capture? and decase?
  (let [inner (case-char p s)]
    (if capture?
      [[:mark] inner [:capture]]
      [inner])))

(defmethod compile :parka/string
  [target {:keys [capture?] :as s}]
  (let [core (mapv #(case-char % s) target)]
    (if capture?
      (into [] (concat [[:mark]] core [[:capture]]))
      core)))

(defmethod compile :parka/any
  [_ {:keys [capture?]}]
  (if capture?
    [[:mark] [:any] [:capture]]
    [[:any]]))

(defmethod compile :parka/seq
  [ps {:keys [capture?] :as s}]
  (let [pre (when capture?
              [[:push []]])
        compiled (mapcat (if capture?
                           #(into (compile % s) [[:apply-capture-2 conj]])
                           #(compile % s))
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

(defmethod compile :parka/alt
  [{:parka/keys [alts]} s]
  (compile-alt alts s))

(defmethod compile :parka/not
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  ; No need to capture lookahead.
  (let [p (compile inner (assoc s :capture? false))]
    (into []
          (concat [[:choice (+ 2 (count p))]]
                  p
                  [[:fail-twice]]
                  (when capture? [[:push nil]])))))

(defmethod compile :parka/and
  [{:parka/keys [inner]} s]
  ; No need to capture lookahead.
  (let [p (compile inner (assoc s :capture? false))]
    (into []
          (concat [[:choice (+ 2 (count p))]]
                  p
                  [[:back-commit 2]
                   [:fail]]))))

(defmethod compile :parka/set
  [chs {:keys [capture? decase?]}]
  (let [chs (if decase?
              (set (concat (map (comp first str/upper-case) chs)
                           (map (comp first str/lower-case) chs)))
              chs)]
    (into [] (concat (when capture? [[:mark]])
                     [[:charset chs]]
                     (when capture? [[:capture]])))))

(defmethod compile :parka/nonterminal
  [nonterminal {:keys [decase?]}]
  (when decase?
    (throw (ex-info "(ic :nonterminal) is unsupported - keep ic just around literals."
                    {:nonterminal nonterminal})))
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

(defmethod compile :parka/grammar
  [{:parka/keys [rules start]} s]
  (let [[labels code] (compile-grammar rules
                                       {:parka/top 0}
                                       [[:open-call start]
                                        [:open-jump :parka/end]]
                                       (assoc s :capture? true))
        labels'       (assoc labels :parka/end (count code))]
    (resolve-calls code labels')))

(defmethod compile :parka/star
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  (let [p (compile inner s)
        cap-width (if capture? 1 0)]
    (into []
          (concat (when capture? [[:push []]])
                  [[:choice (+ 2 cap-width (count p))]]
                  p
                  (when capture? [[:apply-capture-2 conj]])
                  [[:partial-commit (- (+ cap-width (count p)))]]))))

(defmethod compile :parka/drop
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  (into [] (concat (compile inner (assoc s :capture? false))
                   (when capture? [[:push nil]]))))

(defmethod compile :parka/action
  [{:parka/keys [inner action]} {:keys [capture?] :as s}]
  (into [] (concat (compile inner s)
                   (when capture? [[:apply-capture-1 action]]))))

(defmethod compile :parka/ic
  [{:parka/keys [inner]} s]
  (compile inner (assoc s :decase? true)))
