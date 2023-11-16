(ns parka.errors
  (:require
    [clojure.string :as str]))

(defn expectations [err]
  (-> err ex-data :parka/expectations))

(defn pretty-location
  [[filename input pos]]
  (let [prefix (if (pos? pos)
                 (subs input 0 pos)
                 "")
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

