(ns parka.dynamic
  "An alternative the PEG parsing machine, this is a dynamic, recursive
  descent-like PEG parser that consumes Parka's AST.
  (The data structures constructed by [[parka.api]] for the compiler.)"
  (:require
   [clojure.string :as str]
   [parka.ast :as ast]
   [parka.reflect :as reflect]))

(defn- at [{:keys [input pos]}]
  (if (< pos (count input))
    (nth input pos)
    ))

(defn- next? [s ch]
  (= (at s) ch))

(defn- prefix? [{:keys [input pos]} prefix]
  (let [end (+ pos (count prefix))]
    (and (<= end (count input))
         (= (subs input pos end)
            prefix))))

(defn- in+ [s delta]
  (let [s' (update s :pos + delta)]
    (swap! (:max-pos s) max (:pos s'))
    s'))

(defn- cached [{:keys [cache pos]} rule]
  (-> @cache (get rule) (get pos)))

(defn- memo [{:keys [cache]} rule start value]
  (swap! cache assoc-in [rule start value]))

(defmulti ^:private run-parser*
  "Returns one of:
  - [:success s' value]
  - [:fail (delay details)]  for eg. lookahead failures
  - [:error msg]     for unrecoverable errors that should reach the top"
  (fn [_s p]
    (:parka/type p)))

;; Fail details
;; {:choices  [list-of-choice-rules] ;; innermost first
;;  :pos      i
;;  :expected 
;; }

;; It's common for the failure to really be at, say, a missing ] on a list
;; literal. That comes at the end of a sequence like
;;   [ \[ :ws (p/* :expr) :ws \] ]
;; We really want that rightmost parse error! What we expected there was either
;; another :expr in the p/*, or (whitespace then) the closing ].
;; Having matched the opening [ will make this a deeper parse than any of the
;; other flavours of expression at that spot.
;; We don't *necessarily* need to capture "expected expression or ]"; it's
;; sufficient to say "expected ] found 'return'" or something like that.
;; So that really is the rightmost failure straight up.
;; If several branches of a choice have equally deep errors, we use those.
(defn- run-parser
  [s p]
  #_(prn (reflect/print-expr p))
  #_(prn "at" (subs (:input s) (:pos s)))
  (let [result (run-parser* s (ast/->parser p))]
    (println (reflect/print-expr p) "\n  at: " (subs (:input s) (:pos s)) "\n --> " (pr-str result))
    result))

(defn- fail-exp [s expected]
  {:pos      (:pos s)
   :expected expected})

(defn parse-string [expr source text]
  (let [s0 {:input   text
            :source  source
            :pos     0
            :max-pos (atom 0)
            :cache   (atom {})}]
    (run-parser s0 expr)))

(defmethod run-parser* :parka/char
  [s {ch :parka/char}]
  (if (next? s ch)
    [:success (in+ s 1) (str ch)]
    [:fail (delay (fail-exp s (str ch)))]))

(defmethod run-parser* :parka/string
  [s {target :parka/string}]
  (if (prefix? s target)
    [:success (in+ s (count target)) target]
    [:fail (delay (fail-exp s (str "'" target "'")))]))

(defmethod run-parser* :parka/any
  [{:keys [input pos] :as s} _]
  (if (< pos (count input))
    [:success (in+ s 1) (str (at s))]
    [:fail (delay (fail-exp s "anything"))]))

(defmethod run-parser* :parka/seq
  [s {ps :parka/seq}]
  (loop [[p & ps] ps
         s        s
         ret      (transient [])]
    (if (not p)
      [:success s (persistent! ret)]
      (let [[res s' value :as result] (run-parser s p)]
        (if (= res :success)
          (recur ps s' (conj! ret value))
          result)))))

(defn expectations
  "Either returns the list in `:choices` or `[expected]` for a single
  `:expected`.

  This is intended to be a list of human-readable expectations."
  [fail-details]
  ;; Either a list in :choices or a single :expected. Returns a list of strings.
  (or (:choices fail-details)
      [(:expected fail-details)]))

(defmethod run-parser* :parka/alt
  [s {choices :parka/alts}]
  #_(prn "alt over" choices s)
  (loop [[p & ps] choices
         fails    (transient [])]
    (if p
      (let [{:keys [input pos]} s
            result (run-parser s p)]
        (case (first result)
          :fail    (recur ps (conj! fails result))
          ;; On :success or :error, return that immediately.
          result))
      ;; Now if we're run out of alternatives without any succeeding:
      ;; We fail with the rightmost failure among the alternatives, or a choice
      ;; failure if there is a tie.
      [:fail
       (delay
         (let [by-pos    (->> (persistent! fails)
                              (map (comp deref second))
                              (group-by :pos))
               right     (reduce max (keys by-pos))
               rightmost (get by-pos right)]
           (case (count rightmost)
             1 (first rightmost)
             (delay {:pos     right
                     :choices (mapcat expectations rightmost)}))))])))

(defmethod run-parser* :parka/not
  [s {:parka/keys [inner]}]
  (let [result (run-parser s inner)]
    (case (first result)
      ;; TODO: Summarize the expectations of the successful parser!
      :success [:fail (delay (fail-exp s "lookahead failure"))]
      :fail    [:success s nil]
      :error   result)))

(defmethod run-parser* :parka/and
  [s {:parka/keys [inner]}]
  (let [[res _ value :as result] (run-parser s inner)]
    (case res
      :success [:success s nil]
      result)))

(defmethod run-parser* :parka/label [s {:parka/keys [inner label]}]
  (let [[res details :as result] (run-parser s inner)]
    (if (= res :fail)
      [:fail (delay (-> @details
                        (dissoc :choices)
                        (assoc :expected label)))]
      result)))

(defn- fail-set [s chs]
  (fail-exp s (if (set? chs)
                (str "[" (apply str (sort chs)) "]")
                "<predicate>")))

(defmethod run-parser* :parka/set
  [{:keys [input pos] :as s} {chs :parka/set}]
  (or (when (< pos (count input))
        (let [ch (at s)]
          (when (chs ch)
            [:success (in+ s 1) (str ch)])))
      [:fail (delay (fail-set s chs))]))

#_(defn- humanize [s]
  (str/replace s #"[ _-]+" " "))

#_(defn- humanize-keyword [kw]
  (if (qualified-keyword? kw)
    (str (humanize (namespace kw)) " " (humanize (name kw)))
    (humanize (name kw))))

;; TODO: Is there a sound way to use nonterminal names as better context for
;; naming things? That's plausible but hard to get right.
;; I think the clinching factor is that the inner failure must be:
;; - At the same position as this nonterminal call
;; - Not a handwritten label.
;; (It's a little unclear how to handle :choices here, especially if some are
;; human labels and some are not...)
(defmethod run-parser* :parka/nonterminal
  [{:keys [grammar] :as s} {:parka/keys [nonterminal]}]
  (if-let [rule (get grammar nonterminal)]
    (run-parser s rule)
    [:error (str "grammar error: no such nonterminal " nonterminal)]))

(defmethod run-parser* :parka/grammar
  [{:keys [grammar] :as s} {:parka/keys [rules start]}]
  (let [[res s' value :as result] (run-parser (assoc s :grammar rules)
                                              (get rules start))]
    (if (= res :success)
      ;; Keep the adjusted :pos and the result, but reset the grammar to the
      ;; outer one.
      ;; TODO: This might be better off as copying a subset the other way?
      [:success (assoc s' :grammar grammar) value]
      result)))

(defmethod run-parser* :parka/star
  [s {:parka/keys [inner]}]
  (loop [s   s
         ret (transient [])]
    (let [[res s' value :as result] (run-parser s inner)]
      (case res
        :success (recur s' (conj! ret value))
        :fail    [:success s (persistent! ret)]
        :error   result))))

(defmethod run-parser* :parka/action
  [s {:parka/keys [action inner]}]
  (let [[res s' value :as result] (run-parser s inner)]
    (case res
      :success [:success s' (action value)]
      result)))
