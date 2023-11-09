(ns parka.samples.golang
  (:require
   [parka.api :as p]
   [parka.samples.golang.numbers :as go.numbers]))

(def go-fundamentals
  {:unicode-char    (p/do (p/not \newline) p/any)
   ;; TODO: Unicode
   :unicode-letter  (p/match #?(:clj  #(Character/isLetter %)
                                :cljs #(js/RegExp. "\\p{L}" "u")))
   :unicode-digit   (p/match #?(:clj  #(Character/isDigit %)
                                :cljs #(js/RegExp. "\\p{Nd}" "u")))

   :letter          (p/alt \_ :unicode-letter)
   :decimal-digit   (p/range \0 \9)
   :binary-digit    #{\0 \1}
   :octal-digit     (p/range \0 \7)
   :hex-digit       (set "0123456789abcdefABCDEF")})

(def go-whitespace
  {:line-space   " \t"
   :ws1          " \t\r\n"
   :ws           (p/? (p/+ :ws1))
   :line-comment (p/drop (p/do
                           "//"
                           (p/* (p/do (p/not \newline) p/any))
                           \newline))})

(def go-literals
  {:identifier  (p/let [l  :letter
                        ls (p/* (p/alt :letter :unicode-digit))]
                  (apply str l ls))})

(defn go-grammar []
  (merge go-fundamentals
         go-whitespace
         go-literals
         go.numbers/go-integers
         go.numbers/go-floats
         go.numbers/go-imaginary))
