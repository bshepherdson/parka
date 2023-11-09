(ns parka.machine.peg
  (:require
    [parka.errors :as errs]))


;;; The state is
;;; {:pc   n
;;; :code  [...]
;;; :stack [...]
;;; :input "str"
;;; :pos   i
;;; :caps  []}

; Stack types
; - :choice {:pc, :pos, :caps)
; - :return {:pc}

(defn- pop-exp [s exp-type]
  (let [t (-> s :stack peek :type)]
    (when-not (= t exp-type)
      (throw (ex-info (str "Bad stacking in parsing machine: popped"
                           exp-type "not" t)
                      {:expected exp-type
                       :got      t})))
    (update s :stack pop)))

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


(defn- fail
  "Drain the stack until we find a `:choice` record; restore those values."
  [m err]
  (let [tos (peek (:stack m))]
    (case (:type tos)
      nil     (abort m err)
      :choice (let [{:keys [pc pos caps]} tos]
                (-> m
                    (pop-exp :choice)
                    (assoc :pc pc :pos pos :caps caps)))
      :return (recur (pop-exp m :return) err)
      :label  (recur (pop-exp m :label)  (:label m)))))

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
  [{:keys [pos pc caps] :as m} [_ delta offset]]
  (-> m
      pc+
      (update :stack conj {:type :choice
                           :pc   (+ pc delta)
                           :pos  (- pos (or offset 0))
                           :caps caps})))

(defmethod exec :jump
  [m [_ delta]]
  (pc+ m delta))

(defmethod exec :call
  [{:keys [pc] :as m} [_ delta]]
  (-> m
      (pc+ delta)
      (update :stack conj {:type :return
                           :pc   (inc pc)})))

(defn- pop-labels-to [m target-type]
  (let [tos (-> m :stack peek)
        typ (:type tos)]
    (if (= typ :label)
      (recur (update m :stack pop) target-type)
      [tos (pop-exp m target-type)])))

(defmethod exec :label
  [m [_ label]]
  (update m :stack conj {:type  :label
                         :label label}))

(defmethod exec :return
  [m _]
  (let [[{:keys [pc]} m'] (pop-labels-to m :return)]
    (assoc m' :pc pc)))

(defmethod exec :commit
  [m [_ delta]]
  (-> m
      (pc+ delta)
      (pop-labels-to :choice)
      second))

(defmethod exec :partial-commit
  ;; Semantics here are to update PC by delta, and make the TOS backtracking
  ;; entry point to the current position.
  ;; `[pc0 pos0 [... [pc1 pos1]]] -> [pc0+delta pos0 [...[pc1 pos0]]]`
  [m [_ delta]]
  (let [[tos {:keys [caps pos] :as m}] (pop-labels-to m :choice)]
    (-> m
        (pc+ delta)
        (update :stack conj (assoc tos :pos pos :caps caps)))))

(defmethod exec :back-commit
  ;; Moves PC by delta, pops a backtracking record from the stack, ignores its
  ;; PC but moves the input to its position. Uses its caps and captures, but
  ;; pushes an extra nil as the value of this rule.
  [m [_ delta]]
  (let [[{:keys [caps pos]} m] (pop-labels-to m :choice)]
    (-> m
        (pc+ delta)
        (assoc :pos  pos
               :caps (conj caps nil)))))

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
  (if-let [ch (head m)]
    (if (chs ch)
      (-> m pos+ pc+)
      (fail m (errs/failed-expect m chs)))
    (fail m (errs/parse-error m "unexpected EOF"))))

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

(defn run [code label text]
  (loop [m {:pc       0
            :code     code
            :stack    []
            :input    text
            :filename label
            :pos      0
            :caps     []}]
    #_(println (str (:pc m) " " (nth (:code m) (:pc m))
                    "\n    " (:caps m)
                    "\n    " (:input m)
                    "\n    " (apply str (repeat (:pos m) \space)) "^"))
    (if (:done? m)
      m
      (recur (exec m (nth (:code m) (:pc m)))))))

