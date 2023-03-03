(ns parka.test-util
  (:require
   [clojure.test :refer [assert-expr do-report]]))

(defmethod clojure.test/assert-expr 'ex-info?
  [msg [_ desc data & body :as form]]
  `(try
     ~@body
     (do-report {:type :fail :message ~msg :expected '~form :actual nil})
     (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e#
       (let [m# (ex-message e#)
             d# (ex-data e#)]
         (if (and (= m# ~desc)
                  (= d# ~data))
           (do-report {:type :pass :message ~msg :expected '~form :actual e#})
           (do-report {:type :fail :message ~msg :expected '~form :actual e#})))
       e#)))
