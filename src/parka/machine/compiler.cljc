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

(defmethod compile :parka/char
  [p {:keys [capture?]}]
  (if capture?
    [[:char p] [:push (str p)]]
    [[:char p]]))

(defmethod compile :parka/string
  [target {:keys [capture?]}]
  (let [core (mapv #(vector :char %) target)]
    (if capture?
      (into [] (concat [[:mark]] core [[:capture]]))
      core)))

(defmethod compile :parka/any
  [_ {:keys [capture?]}]
  (if capture?
    [[:mark] [:any] [:capture]]
    [[:any]]))

#_(defn- append-seq-result [label caps value]
    (let [old (get caps label)]
      (cond
        (nil?    old) (assoc caps label value)
        (vector? old) (update caps label conj value)
        :else         (assoc caps label [old value]))))

#_(defn- compile-seq-item-cap
    "Compiles a single item inside a seq, when capturing."
    [p s]
    (cond
    ;; If it's a keyword, use the terminal name as the label.
      (keyword? p) (let [compiled (compile p s)]
                     (into (compile p s)
                           [[:apply-capture-2 (partial append-seq-result p)]]))

    ;; If it's a vector, it's just inlined with the same logic.
      (vector? p)  (compile p (assoc s :nested? true))

    ;; For a map with one value, take it as {label target}.
      (and (map? p) (= 1 (count p)))
      (let [[label inner] (first p)
            compiled      (compile inner s)
            f             (partial append-seq-result label)]
        (into compiled [[:apply-capture-2 f]]))

    ;; For any other parser, use the key :parka/matches
      :else   (let [compiled     (compile p s)
                    f            (partial append-seq-result :parka/matches)]
                (into compiled [[:apply-capture-2 f]]))))

#_(defn- compile-seq-item-drop
    [p s]
    (prn "CSID" p)
    (if (and (map? p) (not (:parka/type p)))
      (compile (first (vals p)) s)
      (compile p s)))

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
  [chs {:keys [capture?]}]
  (into [] (concat (when capture? [[:mark]])
                   [[:charset chs]]
                   (when capture? [[:capture]]))))

(defmethod compile :parka/nonterminal
  [nonterminal _]
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

;; ------------------------- Caes-insensitive --------------------------------
(defmulti ^:private decase
  "Converts a parser to be case-insensitive, depending on its type."
  parser-type)

(defmethod compile :parka/ic
  [{:parka/keys [inner]} s]
  (compile (decase inner) s))

;; Many of the parsers are transparent, and just need to update :parka/inner.
(defmethod decase :default [p]
  (update p :parka/inner decase))

(defmethod decase :parka/alt [{:parka/keys [alts] :as p}]
  (assoc p :parka/alts (map decase alts)))

;; Nothing to do for any.
(defmethod decase :parka/any [p]
  p)

(defmethod decase :parka/char [ch]
  (let [lc (first (str/lower-case ch))
        uc (first (str/upper-case ch))
        cs #{lc uc}]
    (if (= (count cs) 1)
      ch
      cs)))

(defmethod decase :parka/grammar [g]
  (update g :parka/rules update-vals decase))

(defmethod decase :parka/ic [{:parka/keys [inner]}]
  ;; No need to nest ICs; they can be unwrapped.
  (decase inner))

(defmethod decase :parka/nonterminal [nt]
  ;; With this kind of indirection, we can't easily do this, so just throw at
  ;; construction time.
  (throw (ex-info "(ic ...) can't (recursively) contain a :nonterminal"
                  {:nonterminal nt})))

(defmethod decase :parka/seq [s]
  (map decase s))

(defmethod decase :parka/set [chs]
  (let [lc (map str/lower-case chs)
        uc (map str/upper-case chs)]
    (->> (concat lc uc)
         (map first) ; str/lower-case converts to a string
         set)))

(defmethod decase :parka/string [s]
  ;; This one is a bit tricky. Map decase over the individual characters, then wrap it
  ;; as an action with str/join.
  {:parka/type   :parka/action
   :parka/inner  (map decase s)
   :parka/action str/join})
