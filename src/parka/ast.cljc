(ns parka.ast)

(defn ->parser [pat]
  (cond
    (map?     pat)                pat
    (keyword? pat)                {:parka/type        :parka/nonterminal
                                   :parka/nonterminal pat}
    #?(:portal  (or (char? pat)
                    (portal.ui.inspector/char? pat))
       :default (char? pat))
    {:parka/type :parka/char   :parka/char   pat}
    (string?  pat)                {:parka/type :parka/string :parka/string pat}
    (vector?  pat)                {:parka/type :parka/seq    :parka/seq    pat}
    (set?     pat)                {:parka/type :parka/set    :parka/set    pat}
    :else (throw (ex-info "bad ->parser pattern" {:value pat
                                                 :value-str (pr-str pat)}))))
