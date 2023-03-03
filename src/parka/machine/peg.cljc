(ns parka.machine.peg
  (:require
   [clojure.string :as str]
   [parka.errors :as errs]))

;;; The state is
;;; {:pc     n
;;; :code    [...]
;;; :stack   [...]
;;; :context {}
;;; :input   "str"
;;; :pos     i
;;; :caps    []}

;; An ideal error message should explain what it was expecting to find (possibly
;; several things), and what it found instead, with a pointer to the location.
;; That's easy enough for a literal character, set or string.
;; For a sequence, just whatever it was trying to find next?
;; For an alt, we want to collect a list of all the paths, 

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

(defn- error-context
  ([ctx]
   (if (empty? ctx)
     ""
     (error-context (peek ctx) (pop ctx))))

  ([base ctx]
   ;; If there are real human labels, use only those.
   ;; If there are no human labels, use up to the last 3 "soft", automatic labels.
   (let [labels (remove :parka/soft ctx)
         src    (if (empty? labels)
                  (take-last 3 (keep :parka/soft ctx))
                  labels)
         msg    (or (:parka/message base) base)]
     (str (str/join (for [label src]
                      (str "Error parsing " label ": ")))
          base))))

(defn- abort [m err]
  (assoc m
         :done? true
         :error (do
                  (prn "abort error" err)
                  (if (= err ::expected-failure)
                    (errs/parse-error m (error-context (:context m)))
                    (update err :parka/message error-context (:context m))))))

(defn- fail
  "Drain the stack until we find a `[pc' pos' caps']` pair; set these values."
  [m err]
  (let [tos (peek (:stack m))]
    (if (nil? tos)
      (abort m err) ; We've run out of stack - a failed parse.
      (let [m' (update m :stack pop)]
        (if (map? tos)
          (let [{:keys [pc pos caps]} tos]
            (assoc m' :pc pc :pos pos :caps caps))
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
  [{:keys [pos pc caps] :as m} [_ delta offset]]
  (-> m
      pc+
      (update :stack conj {:pc   (+ pc delta)
                           :pos  (- pos (or offset 0))
                           :caps caps})))

(defmethod exec :jump
  [m [_ delta]]
  (pc+ m delta))

(defmethod exec :call
  [{:keys [pc pos] :as m} [_ delta]]
  (-> m
      (pc+ delta)
      (update :stack conj (inc pc))))

(defmethod exec :return
  [{:keys [stack] :as m} _]
  (-> m
      (assoc :pc (peek stack))
      (update :stack pop)))

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
        (assoc :pos  (:pos tos)
               :caps (conj (:caps tos) nil)))))

(defmethod exec :context
  [m [_ label]]
  (-> m
      pc+
      (update :context conj label)))

(defmethod exec :context-mark
  [m _]
  (-> m
      pc+
      (update :context conj {:parka/lookahead (:pos m)})))

(defmethod exec :pop-context
  [m _]
  (-> m
      pc+
      (update :context pop)))

(defmethod exec :fail
  [m [_ err]]
  (fail m (or err ::expected-failure)))

(defmethod exec :not-succeeded
  ;; At the start of the not clause, the start of the lookahead was [:context-mark]ed.
  ;; If the lookahead fails as expected, it'll revert to the earlier [:choice], which
  ;; rests the context.
  ;; However if we make it here, then the character at the marked position was the
  ;; first one that was unexpected.
  [{:keys [caps input] :as m} [_ err]]
  (let [mark  (peek caps)
        found (str "found unexpected " (or err (nth input mark)))]
    (fail (update m :stack pop) found)))

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

(defn run [{code :parser tokens :tokens} label text]
  (loop [m {:pc       0
            :code     code
            :stack    []
            :context  []
            :tokens   tokens
            :input    text
            :filename label
            :pos      0
            :caps     []}]
    #_(prn (:pc m) (nth (:code m) (:pc m)) (:caps m))
    (if (:done? m)
      m
      (recur (exec m (nth (:code m) (:pc m)))))))

