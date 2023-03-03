(ns parka.errors
  (:require
   [clojure.string :as str]
   [parka.util :as u]))

(defn expectations [err]
  (-> err ex-data :parka/expectations))

(defn pretty-location
  [[filename input pos]]
  (let [prefix (subs input 0 pos)
        lines  (str/split-lines prefix)
        line   (count lines)
        col    (count (last lines))]
    (str filename " line " line " col " col)))

(defn- location [{:keys [filename pos input]}]
  [filename input pos])

(defn parse-error
  [loc msg]
  {:parka/parse-error true
   :parka/loc         (location loc)
   :parka/message     msg})

(defn failed-expect
  [loc expectations]
  {:parka/parse-error  true
   :parka/loc          (location loc)
   :parka/message      "failed expectation"
   :parka/expectations expectations})

(defn failed-expect-msg
  [loc msg expectations]
  {:parka/parse-error  true
   :parka/loc          (location loc)
   :parka/message      msg
   :parka/expectations expectations})

;; ----------------------------- Expectations --------------------------------
(defmulti expectation (fn [p _] (u/parser-type p)))

(defmethod expectation :default [{:parka/keys [inner]} s]
  (expectation inner s))

(defmethod expectation :parka/char [p _]
  (str "'" p "'"))

(defmethod expectation :parka/string [target _]
  (pr-str target))

(defmethod expectation :parka/set [chs _]
  (set (for [ch (sort chs)]
         (str "'" ch "'"))))

(defmethod expectation :parka/any [_ _]
  "anything")

(defmethod expectation :parka/seq [ps s]
  (expectation (first ps) s))

(defmethod expectation :parka/alt [{:parka/keys [alts]} s]
  (set (map #(expectation % s) alts)))

(defmethod expectation :parka/not [{:parka/keys [inner]} s]
  (if (-> inner :parka/type (= :parka/any))
    ;; Special case for EOF
    (str "Expected EOF, but got " (let [token (nth (:input s) (:pos s))]
                                    (or (:parka/label token)
                                        (str token))))
    (str "Expected not to be followed by " (expectation inner s))))

(defmethod expectation :parka/and [{:parka/keys [inner]} s]
  (str "a following " (expectation inner s)))

(defmethod expectation :parka/nonterminal [nonterminal s]
  ;; Two cases here: terminals (ie. tokens) and nonterminals (ie. grammar tags).
  (let [label (some-> s :tokens nonterminal)]
    (or label
        (name nonterminal))))

(defmethod expectation :parka/grammar [{:parka/keys [rules start]} s]
  (expectation (get rules start) s))

(defmethod expectation :parka/labeled [{:parka/keys [label]} _]
  label)
