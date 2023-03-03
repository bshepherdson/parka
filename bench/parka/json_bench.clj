(ns parka.json-bench
  (:require
   [clj-async-profiler.core :as prof]
   [parka.api :as p]
   [parka.json-test :as json]))

(defn profile-pretty-sample []
  (let [parser  (p/compile (p/grammar json/json-char-rules :start))
        content (slurp "./pretty.json")]
    (prof/start {})
    (p/parse parser "<test>" content)
    (prof/stop {})))

(comment
  (profile-pretty-sample))
