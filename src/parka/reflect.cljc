(ns parka.reflect
  (:require
   [parka.ast :as ast]))

(defmulti print-expr*
  (fn [p]
    (:parka/type p)))

(defn print-expr [p]
  (print-expr* (ast/->parser p)))

(defmethod print-expr* :parka/char [{ch :parka/char}]
  ch)

(defmethod print-expr* :parka/string [{target :parka/string}]
  target)

(defmethod print-expr* :parka/any [_]
  "<any>")

(defmethod print-expr* :parka/seq [{ps :parka/seq}]
  (mapv print-expr ps))

(defmethod print-expr* :parka/alt [{choices :parka/alts}]
  (into [:alt] (map print-expr choices)))

(derive :parka/not   :parka/wrappers)
(derive :parka/and   :parka/wrappers)
(derive :parka/star  :parka/wrappers)
(derive :parka/label :parka/wrappers)

(defmethod print-expr* :parka/wrappers [{:parka/keys [inner type]}]
  [(keyword (name type)) (print-expr inner)])

(defmethod print-expr* :parka/set [{chs :parka/set}]
  chs)

(defmethod print-expr* :parka/nonterminal [{:parka/keys [nonterminal]}]
  nonterminal)

(defmethod print-expr* :parka/grammar [{:parka/keys [rules start]}]
  [:grammar start #_rules])

(defmethod print-expr* :parka/action [{:parka/keys [action inner]}]
  [:action (print-expr inner) action])
