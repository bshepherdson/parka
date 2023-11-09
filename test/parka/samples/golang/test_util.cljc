(ns parka.samples.golang.test-util
  (:require
   [parka.api :as p]
   [parka.samples.golang :as golang]))

(defn parse-from [expr input]
  (let [exprs  (merge (golang/go-grammar)
                      {::test (p/value [expr p/eof] first)})
        result (p/parse (p/compile (p/grammar exprs ::test))
                        "<test>" input)]
    (if (contains? result :success) ; nil is a valid result!
      (:success result)
      result)))

(defn parse [input]
  (parse-from :start input))
