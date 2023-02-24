(ns parka.machine.peg
  (:require
    [parka.errors :as errs]))

(defn- head [^clojure.lang.IPersistentMap m]
  (let [input ^String (.valAt m :input)
        pos   ^long   (.valAt m :pos)]
    (if (>= pos (.length input))
      nil
      (.charAt input pos))))

(defn- pos+
  ([m] (pos+ m 1))
  ([m n] (update m :pos + n)))

(defn- pc+
  ([m] (pc+ m 1))
  ([m n] (update m :pc + n)))

(defn- end [m]
  (assoc m :done? true))

(defn- abort [m err]
  (assoc m :done? true :error err))

(defn- cache-miss [err pos m [_ sym]]
  (assoc-in m [:cache pos sym] {::cached-failure true :error err}))

(defn- fail
  "Drain the stack until we find a `{pc' pos' caps'...}` map; restore these
  saved values.
  We check the current and saved memos, any current-only memos get cached
  failures added."
  [m err]
  (let [tos (peek (:stack m))]
    (if (nil? tos)
      (abort m err) ; We've run out of stack - a failed parse.
      (let [m' (update m :stack pop)]
        (if (map? tos)
          (let [{:keys [caps memos pc pos]} tos
                live-memos (into #{} memos)
                dead-memos (filter (complement live-memos) (:memos m))]
            (-> (reduce (partial cache-miss err pos) m' dead-memos)
                (assoc :pc pc :pos pos :caps caps :memos memos)))
          (recur m' err))))))

(defmulti exec (fn [_ [op]] op))

(defmethod exec :char [m [_ ch]]
  (if (= (head m) ch)
    (-> m pos+ pc+)
    (fail m (errs/failed-expect m [(str ch)]))))

(defmethod exec :any
  [{:keys [input pos] :as m} _]
  (if (< pos (count input))
    (-> m pos+ pc+)
    (fail m (errs/parse-error m "unexpected EOF"))))

(defmethod exec :choice
  [{:keys [memos pos pc caps] :as m} [_ delta offset]]
  (-> m
      pc+
      (update :stack conj {:pc    (+ pc delta)
                           :pos   (- pos (or offset 0))
                           :caps  caps
                           :memos memos})))

(defmethod exec :jump
  [m [_ delta]]
  (pc+ m delta))

(defmethod exec :call
  [{:keys [pc] :as m} [_ delta]]
  (-> m
      (pc+ delta)
      (update :stack conj (inc pc))))

(defmethod exec :commit
  [m [_ delta]]
  (-> m
      (pc+ delta)
      (update :stack pop)))

(defmethod exec :partial-commit
  ;; Semantics here are to update PC by delta, and make the TOS backtracking entry
  ;; point to the current position.
  ;; `[pc0 pos0 [... [pc1 pos1]]] -> [pc0+delta pos0 [...[pc1 pos0]]]`
  [m [_ delta]]
  (-> m
      (pc+ delta)
      (update :stack #(conj (pop %) (assoc (peek %)
                                           :pos (:pos m)
                                           :caps (:caps m))))))

(defmethod exec :back-commit
  ;; Moves PC by delta, pops a backtracking record from the stack, ignores its
  ;; PC but moves the input to its position. Uses its caps and captures, but
  ;; pushes an extra nil as the value of this rule.
  [m [_ delta]]
  (let [tos (peek (:stack m))]
    (-> m
        (pc+ delta)
        (update :stack pop)
        (assoc :pos   (:pos tos)
               :caps  (conj (:caps tos) nil)
               :memos (:memos tos)))))

(defmethod exec :fail
  [m [_ err]]
  (fail m (or err ::expected-failure)))

(defmethod exec :fail-twice
  [m [_ err]]
  (fail (update m :stack pop) (or err ::expected-failure)))

(defmethod exec :end
  [m _]
  (end m))

(defmethod exec :charset
  [m [_ chs]]
  (if (chs (head m))
    (-> m pos+ pc+)
    (fail m (errs/failed-expect m chs))))

(defmethod exec :span
  ;; Span is a run of character-set values.
  ;; It works like :charset but doesn't advance PC until a non-matching char.
  [m [_ chs]]
  (if (chs (head m))
    (pos+ m)
    (pc+ m)))

(defmethod exec :test-char
  [m [_ ch delta]]
  (if (= ch (head m))
    (-> m pc+ pos+)
    (pc+ m delta)))

(defmethod exec :test-any
  [m [_ n delta]]
  (let [pos' (+ (:pos m) n)]
    (if (<= pos' (count (:input m)))
      (-> m pc+ (pos+ n))
      (pc+ m delta))))

(defmethod exec :test-charset
  [m [_ chs delta]]
  (if (chs (head m))
    (-> m pc+ pos+)
    (pc+ m delta)))

;;; Capturing and stack management
(defmethod exec :drop
  [m _]
  (-> m
      pc+
      (update :caps pop)))

(defmethod exec :push
  [m [_ value]]
  (-> m
      pc+
      (update :caps conj value)))

(defmethod exec :mark
  [{:keys [pos] :as m} _]
  (-> m
      pc+
      (update :caps conj pos)))

(defmethod exec :capture
  [{:keys [input pos caps] :as m} _]
  (let [captured (subs input (peek caps) pos)]
    (-> m
        pc+
        (assoc :caps (conj (pop caps) captured)))))

(defmethod exec :apply-capture-1
  [{:keys [caps] :as m} [_ f]]
  (let [tos' (f (peek caps))]
    (-> m
        pc+
        (assoc :caps (conj (pop caps) tos')))))

(defmethod exec :apply-capture-2
  [{:keys [caps] :as m} [_ f]]
  (let [tos    (peek caps)
        caps1  (pop  caps)
        nos    (peek caps1)
        caps2  (pop  caps1)
        tos'   (f nos tos)]
    (-> m
        pc+
        (assoc :caps (conj caps2 tos')))))

;;; Memoization scheme:
;;; At the tops of each expression in a grammar, the instruction  [:memo sym]
;;; is generated. On exiting the expression successfully, it runs [:return-memo]
;;; to signal the end.
;;;
;;; The bookkeeping is done with a stack of memoization state, similar to
;;; captures. [:memo sym] pushes [pos sym] pairs.
;;; - The cache of completed memoizations is permanent - it doesn't change for a
;;;   particular input string.
;;; - However the stack of memoizations is saved and restored by :choice.
(defn- return
  [{:keys [stack] :as m}]
  (-> m
      (assoc :pc (peek stack))
      (update :stack pop)))

(defmethod exec :return
  [m _]
  (return m))

(defmethod exec :memo
  [{:keys [cache pos] :as m} [_ sym]]
  ; TODO This doesn't let grammars nest - there's only one cache and they can
  ; collide.
  (let [cached (get-in cache [pos sym])]
    ; There are three cases here: cached success, cached failure, not cached.
    (cond
      (::cached-failure cached)
      (fail m (:error cached))

      cached
      (-> m
          (update :caps conj (:result cached))
          (assoc :pos (:pos cached))
          return)

      :else
      (-> m
          pc+
          (update :memos conj [pos sym])))))

(defmethod exec :return-memo
  [{:keys [caps memos pos] :as m} _]
  (let [[start sym] (peek memos)]
    #_(prn "return-memo" start sym caps)
    (-> m
        (update :memos pop)
        (assoc-in [:cache start sym] {:pos pos :result (peek caps)})
        return)))

(defn run [code label text]
  (let [loops (atom 0)]
    (loop [m {:pc       0
              :code     code
              :stack    []
              :input    text
              :filename label
              :pos      0
              :memos    []  ; Stack of [pos :rule] pairs for memoization.
              :cache    {}  ; {pos {:rule {:pos ... :result ...}}}
              :caps     []}]
      ;(println "================================")
      ;(prn (:pc m) (nth (:code m) (:pc m)))
      ;(prn (subs (:input m) (:pos m)))
      ;(println "Memos:")
      ;(doseq [[pos rule] (:memos m)]
      ;  (printf "  %-4d: %s\n" pos (str rule)))
      ;(prn (:cache m))
      ;(println "(enter)")
      ;(read-line)

      #_(when (zero? (mod (swap! loops inc) 1000000))
          (prn (->> m :stack (filter map?) first :pos)))
      (if (:done? m)
        m
        (recur (exec m (nth (:code m) (:pc m))))))))

