(ns parka.dynamic
  "Dynamic, recursive descent evaluator for Parka.
  Not as fast as the previous PEG version, but it works nicely with tokenization and
  produces better error messages."
  (:require
   [clojure.string :as str]
   [parka.errors :as errs]
   [parka.util :as u]))

(defmulti ^:private -evaluate (fn [p _] (u/parser-type p)))

(defn- fetch
  ([s expected]
   (fetch s expected (pr-str expected)))
  ([{:keys [input pos]} expected exp-msg]
   (if (< pos (count input))
     (nth input pos)
     (throw (ex-info (str "Unexpected EOF! Expected " exp-msg)
                     {:expected expected})))))

(defn- value [token-or-char]
  (get token-or-char :parka/value
       (if (char? token-or-char)
         (str token-or-char)
         token-or-char)))

(defn- pos+
  ([s] (pos+ s 1))
  ([s delta] (update s :pos + delta)))

(defn- fail [err]
  (throw (ex-info "Parse error" err)))

(defmethod -evaluate :parka/char
  [p {:keys [capture?] :as s}]
  (let [ch (fetch s p)]
    (if (= p ch)
      [(pos+ s) (when capture? (str p))]
      (throw (ex-info (str "Expected " (pr-str p) ", got " (pr-str ch))
                      {:expected p :got ch})))))

(defmethod -evaluate :parka/string
  [target {:keys [capture? input pos] :as s}]
  (let [end  (+ pos (count target))
        mate (when (<= end (count input))
               (str/join (subs input pos end)))]
    (cond
      (nil? mate)     (throw (ex-info
                              (str "Unexpected EOF! Expected " (pr-str target))
                              {:expected target}))
      (= target mate) [(pos+ s (count target))
                       (when capture? target)]
      :else           (throw (ex-info
                              (str "Expected " (pr-str target) ", got " (pr-str mate))
                              {:expected target :got mate})))))

(defmethod -evaluate :parka/set
  [chs {:keys [capture?] :as s}]
  (let [ch (fetch s chs (str "one of " (pr-str chs)))]
    (if (chs ch)
      [(pos+ s) (str ch)]
      (throw (ex-info (str "Expected one of " (pr-str chs) ", got " ch)
                      {:expected chs :got ch})))))

(defmethod -evaluate :parka/any
  [_ {:keys [capture?] :as s}]
  (when-let [ch (fetch s :any "anything")]
    [(pos+ s) (when capture? (value ch))]))

(defmethod -evaluate :parka/seq
  [ps {:keys [capture?] :as s}]
  (loop [[p & ps] ps
         s        s
         res      (when capture? (transient []))]
    (let [[s value] (-evaluate p s)
          res       (when capture? (conj! res value))]
      (if (empty? ps)
        [s (when capture? (persistent! res))]
        (recur ps s res)))))

(defn- -evaluate-alt [alts s]
  (loop [[alt & alts] alts]
    (let [inner (try
                  (-evaluate alt s)
                  (catch #?(:clj Exception :cljs js/Error) _
                    nil))]
      (cond
        ;; If this alternative succeeded, return it.
        inner inner
        ;; If all alternatives have failed, return nil and the parent `-evaluate` will throw.
        (empty? alts) nil
        ;; If it failed, but there are more, keep looping.
        :else (recur alts)))))

(defmethod -evaluate :parka/alt
  [{:parka/keys [alts] :as p} s]
  (let [res (-evaluate-alt alts s)]
    (if (nil? res)
      (let [exps (distinct (map #(errs/expectation % s) alts))]
        (throw (ex-info (str "Expected one of: " (str/join ", " exps))
                        {:expected exps})))
      res)))

(defmethod -evaluate :parka/not
  [{:parka/keys [inner] :as p} {:keys [capture?] :as s}]
  ; No need to capture lookahead.
  (let [res (try
              (-evaluate inner (assoc s :capture? false))
              (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) _
                nil))]
    (if res
      (let [exp (errs/expectation p s)]
        (throw (ex-info exp {:expected {:not (errs/expectation inner s)}})))
      [s nil])))

(defmethod -evaluate :parka/and
  [{:parka/keys [inner]} s]
  ; No need to capture lookahead.
  (-evaluate inner s) ; Parse as a side effect, then return nil without advancing.
  [s nil])

(defn- match-terminal [tag label s]
  (let [token (fetch s tag label)]
    (if (= (:parka/token token) tag)
      [(pos+ s) (:parka/value token)]
      (throw (ex-info (str "Expected " label ", got " (:parka/label token)) {})))))

(defn- match-nonterminal [nt {:keys [rules] :as s}]
  (let [rule (get rules nt)]
    (if rule
      (-evaluate rule s)
      (throw (ex-info (str "No such rule in grammar: " (pr-str nt)) {})))))

(defmethod -evaluate :parka/nonterminal
  [nonterminal s]
  ;; Two cases here: terminals (ie. tokens) and nonterminals (ie. grammar tags).
  (let [label (some-> s :tokens nonterminal)]
    (if label
      (match-terminal    nonterminal label s)
      (match-nonterminal nonterminal s))))

(defmethod -evaluate :parka/grammar
  [{:parka/keys [rules start]} s]
  (if-let [start-rule (get rules (or start :start))]
    (let [inner-s  (assoc s :rules rules)
          [s' res] (-evaluate start-rule inner-s)]
      ;; Need to put back any existing :rules value.
      [(assoc s' :rules (:rules s)) res])
    (throw (ex-info "Start rule of grammar missing" {:rules rules :start start}))))

(defmethod -evaluate :parka/star
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  (loop [res (when capture? (transient []))
         s   s]
    (if-let [[s' res-el] (try
                           (-evaluate inner s)
                           (catch #?(:clj Exception :cljs js/Error) _
                             nil))]
      ;; If it successfully matched, keep going.
      (recur (when capture? (conj! res res-el)) s')
      ;; If it failed, return the previous state and result, if any.
      [s (when capture? (persistent! res))])))

(defmethod -evaluate :parka/drop
  [{:parka/keys [inner]} {:keys [capture?] :as s}]
  (let [[s _] (-evaluate inner (assoc s :capture? false))]
    [(assoc s :capture? capture?) nil]))

(defmethod -evaluate :parka/action
  [{:parka/keys [inner action]} {:keys [capture?] :as s}]
  (let [[s res] (-evaluate inner s)]
    (if capture?
      [s (action res)]
      [s nil])))

(defmethod -evaluate :parka/label
  [{:parka/keys [inner label]} s]
  (-evaluate inner s))

;; TODO: Handle :parka/ic - or remove it and make that a tokenizer problem?
(defn compile
  ([expr] (compile expr nil))
  ([expr tokenizer]
   {:tokens    (some-> tokenizer meta :token-map)
    :tokenizer tokenizer
    :code      expr}))

(defn evaluate
  "Runs the parser over the input, which might be tokens or might be characters."
  [{:keys [code tokens]} filename input]
  (let [[_ result] (-evaluate code {:tokens   tokens
                                    :capture? true
                                    :filename filename
                                    :input    input
                                    :pos      0})]
    result))
