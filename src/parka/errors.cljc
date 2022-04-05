(ns parka.errors
  (:require
    [clojure.string :as str]))

(defn expectations [err]
  (-> err ex-data :parka/expectations))

(defn- file-location
  [{:keys [filename pos input]}]
  (prn "loc" filename pos input)
  (let [prefix (subs input 0 pos)
        lines  (str/split-lines prefix)
        line   (count lines)
        col    (count (last lines))]
    (prn prefix lines line col)
    (str filename " line " line " col " col)))

(defn parse-error
  [loc msg]
  (ex-info (str (file-location loc) ": " msg)
           {:parka/parse-error true
            :parka/message     msg}))

(defn failed-expect
  [loc expectations]
  (ex-info (str (file-location loc) ": expected "
                (str/join " or " expectations))
           {:parka/parse-error true
            :parka/expectations expectations}))

(defn failed-expect-msg
  [loc msg expectations]
  (ex-info (str (file-location loc) ": " msg ", expected "
                (str/join " or " expectations))
           {:parka/parse-error  true
            :parka/message      msg
            :parka/expectations expectations}))

