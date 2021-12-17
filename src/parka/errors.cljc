(ns parka.errors
  (:require
    [clojure.string :as str]))

(defn expectations [err]
  (-> err ex-data :parka/expectations))

(defn- file-location
  [{:keys [filename line col]}]
  (str filename " line " line " col " col))

(defn parse-error
  [loc msg]
  (throw (ex-info (str (file-location loc) ": " msg)
                  {:parka/parse-error true
                   :parka/message     msg})))

(defn failed-expect
  [loc expectations]
  (throw (ex-info (str (file-location loc) ": expected "
                       (str/join " or " expectations))
                  {:parka/parse-error true
                   :parka/expectations expectations})))

(defn failed-expect-msg
  [loc msg expectations]
  (throw (ex-info (str (file-location loc) ": " msg ", expected "
                       (str/join " or " expectations))
                  {:parka/parse-error  true
                   :parka/message      msg
                   :parka/expectations expectations})))


