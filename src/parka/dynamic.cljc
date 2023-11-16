(ns parka.dynamic
  "An alternative the PEG parsing machine, this is a dynamic, recursive
  descent-like PEG parser that consumes Parka's AST.
  (The data structures constructed by [[parka.api]] for the compiler.)"
  (:require
   [clojure.string :as str]
   [clojure.zip :as zip]
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

;; Debugging output: Portal can render just about anything, and we want a custom
;; UI here in any case. So we can choose basically any data format and adapt to
;; it.
;; Primary requirement is to make the entire parse tree walkable.
;; Navigation probably via zipper?
;; Each parsing rule needs adjustment so it keeps track of all its inner parses.
;; The real trick is to do that in such a way that it can be macro'd out.
;; Probably easiest to do it in an Atom accumulating kind of way then, rather
;; than returning the history with the results in metadata or whatever.
;; So the history is like this:
;;   [:enter s p]
;;   [:leave result]
(def ^:private ^:dynamic *history?* false)

(defn- run-parser
  [s p]
  (let [p (ast/->parser p)]
    (when *history?*
      (swap! (:history s) conj [:enter s p]))
    (let [result (run-parser* s p)]
      (when *history?*
        (swap! (:history s) conj [:leave result]))
      result)))

(defn- fail-exp [s expected]
  {:pos      (:pos s)
   :expected expected})

(defn- trim-state [s]
  (select-keys s [:expr :input :pos :source]))

(defn- tree-analysis [history]
  (loop [stack    [{::parse-tree true}]
         [h & hs] history]
    (cond
      (and (not h)
           (= (count stack) 1)) (-> stack first :children first
                                    (assoc ::parse-tree true))
      (not h)                   (throw (ex-info "stacking error" {:stack stack}))

      (= (first h) :enter)
      (recur (conj stack {:state (-> h second trim-state)
                          :expr  (nth h 2)})
             hs)

      (= (first h) :leave)
      (let [tos    (-> (peek stack)
                       (assoc :result (second h)))
            stack' (pop stack)
            nos    (update (peek stack') :children (fnil conj []) tos)]
        (recur (conj (pop stack') nos) hs)))))

(defn- parse-string-inner [expr source text s0]
  (let [s1     (merge {:input   text
                       :source  source
                       :pos     0
                       :max-pos (atom 0)
                       :cache   (atom {})}
                      s0)]
    (run-parser s1 expr)))

(defn parse-string [expr source text]
  (parse-string-inner expr source text nil))

(defn parse-string-debug [expr source text]
  (let [history (atom [])
        parsed  (binding [*history?* true]
                  (parse-string-inner expr source text
                                      {:history history}))]
    (tap> (with-meta (tree-analysis @history)
                     {:portal.viewer/default ::parse-tree}))
    (if (= (first parsed) :success)
      [:success
       (-> (second parsed)
           (dissoc :history))
       (last parsed)]
      parsed)))

;; TODO: This should be smarter about zero-width things.
;; Eg. [ (p/* \a) \b ]   should expect not just "a" or "b" but "a or b", since
;; the * might be empty. [[expected]] should return a kind of "prefix", all the
;; things that might come first.
;; To really capture that, however, it needs to be a lot smarter about things
;; like + and ?, because those are currently compiled as eg. [a (* a)] with an
;; action.
(defmulti ^:private expected*
  "Given the state and a parser, summarizes what this parser was expecting."
  (fn [_s {t :parka/type}] t))

(defn- expected [s p]
  (expected* s (ast/->parser p)))

(comment
  (require '[portal.api :as portal])
  (portal/tap)
  (portal/open)
  (portal/start {:port 50401})
  (tap> 7)
  *e
  (portal/eval-str (slurp (clojure.java.io/resource "parka/portal/tree_viewer.cljs")))
  (portal/eval-str (slurp (clojure.java.io/resource "parka/reflect.cljc"))))

(defmethod run-parser* :parka/char
  [s {ch :parka/char :as p}]
  (if (next? s ch)
    [:success (in+ s 1) (str ch)]
    [:fail (delay (fail-exp s (expected s p)))]))

(defmethod expected* :parka/char [_ {ch :parka/char}]
  (str ch))

(defmethod run-parser* :parka/string
  [s {target :parka/string :as p}]
  (if (prefix? s target)
    [:success (in+ s (count target)) target]
    [:fail (delay (fail-exp s (expected s p)))]))

(defmethod expected* :parka/string
  [_ {target :parka/string}]
  (str "'" target "'"))

(defmethod run-parser* :parka/any
  [{:keys [input pos] :as s} p]
  (if (< pos (count input))
    [:success (in+ s 1) (str (at s))]
    [:fail (delay (fail-exp s (expected s p)))]))

(defmethod expected* :parka/any [_ _]
  "anything")

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

(defmethod expected* :parka/seq
  [s {ps :parka/seq}]
  ;; Simply returns the expectation of the first parser.
  ;; TODO: That might be dumb if the first parser is a lookahead etc.
  (expected s (first ps)))

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
  (loop [[p & ps] choices
         fails    (transient [])]
    (if p
      (let [result (run-parser s p)]
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
             {:pos     right
              :choices (mapcat expectations rightmost)})))])))

(defmethod expected* :parka/alt
  [s {choices :parka/alts}]
  (let [exps (map #(expected s %) choices)]
    (str (str/join ", " (butlast exps))
         (when (> (count exps) 2) ",")
         "or "
         (last exps))))

(defmethod run-parser* :parka/not
  [s {:parka/keys [inner] :as p}]
  (let [result (run-parser s inner)]
    (case (first result)
      ;; TODO: Summarize the expectations of the successful parser!
      :success [:fail (delay (fail-exp s (expected s p)))]
      :fail    [:success s nil]
      :error   result)))

(defmethod expected* :parka/not
  [s {:parka/keys [inner]}]
  (str "not " (expected s inner)))

(defmethod run-parser* :parka/and
  [s {:parka/keys [inner]}]
  (let [[res _ value :as result] (run-parser s inner)]
    (case res
      :success [:success s nil]
      result)))

(defmethod expected* :parka/and
  [s {:parka/keys [inner]}]
  (expected s inner))

(defmethod run-parser* :parka/label [s {:parka/keys [inner label]}]
  (let [[res details :as result] (run-parser s inner)]
    (if (= res :fail)
      [:fail (delay (-> @details
                        (dissoc :choices)
                        (assoc :expected label)))]
      result)))

(defmethod expected* :parka/label
  [_ {:parka/keys [label]}]
  label)

(defmethod run-parser* :parka/set
  [{:keys [input pos] :as s} {chs :parka/set :as p}]
  (or (when (< pos (count input))
        (let [ch (at s)]
          (when (chs ch)
            [:success (in+ s 1) (str ch)])))
      [:fail (delay (fail-exp s (expected s p)))]))

(defmethod expected* :parka/set
  [_ {chs :parka/set}]
  (if (set? chs)
    (str "[" (apply str (sort chs)) "]")
    "<predicate>"))

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

(defmethod expected* :parka/nonterminal
  [{:keys [grammar] :as s} {:parka/keys [nonterminal]}]
  (expected s (get grammar nonterminal)))

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

(defmethod expected* :parka/grammar
  [s {:parka/keys [rules start]}]
  (expected (assoc s :grammar rules)
            (ast/->parser start)))

(defmethod run-parser* :parka/star
  [s {:parka/keys [inner]}]
  (loop [s   s
         ret (transient [])]
    (let [[res s' value :as result] (run-parser s inner)]
      (case res
        :success (recur s' (conj! ret value))
        :fail    [:success s (persistent! ret)]
        :error   result))))

(defmethod expected* :parka/star
  [s {:parka/keys [inner]}]
  ;; TODO: "0 or more"?
  (expected s inner))

(defmethod run-parser* :parka/action
  [s {:parka/keys [action inner]}]
  (let [[res s' value :as result] (run-parser s inner)]
    (case res
      :success [:success s' (action value)]
      result)))

(defmethod expected* :parka/action
  [s {:parka/keys [inner]}]
  (expected s inner))
