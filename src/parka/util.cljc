(ns parka.util)

(defn parser-type [pat]
  (cond
    (map?        pat) (:parka/type pat)
    (keyword?    pat) :parka/nonterminal
    (char?       pat) :parka/char
    (string?     pat) :parka/string
    (set?        pat) :parka/set
    (sequential? pat) :parka/seq
    :else (throw (ex-info "bad compile pattern" {:value pat}))))
