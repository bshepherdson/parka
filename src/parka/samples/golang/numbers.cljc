(ns parka.samples.golang.numbers
  (:require
   [parka.api :as p]))

(defn- parse-int [s base]
  #?(:clj  (Long/parseLong s base)
     :cljs (js/parseInt s base)))

(defn- parse-float [s]
  #?(:clj  (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(defn- number-base [prefix digits-rule base]
  (p/do
    \0 prefix
    (-> digits-rule
        p/str
        (p/value #(parse-int % base)))))

(def go-integers
  {:int-lit     (p/alt :binary-lit :octal-lit :hex-lit :decimal-lit)
   :decimal-lit (p/alt (p/value \0 (constantly 0))
                       (p/let [d  (p/range \1 \9)
                               ds (p/? :decimal-digits)]
                         (parse-int (apply str d ds) 10)))
   :decimal-digits (p/str (p/+ (p/do (p/? \_) :decimal-digit)))
   :binary-digits  (p/str (p/+ (p/do (p/? \_) :binary-digit)))
   :octal-digits   (p/str (p/+ (p/do (p/? \_) :octal-digit)))
   :hex-digits     (p/str (p/+ (p/do (p/? \_) :hex-digit)))
   :binary-lit     (number-base #{\b \B}       :binary-digits 2)
   :octal-lit      (number-base (p/? #{\o \O}) :octal-digits  8)
   :hex-lit        (number-base #{\x \X}       :hex-digits    16)})

(def go-floats
  {:float-lit         :decimal-float-lit #_(p/alt :decimal-float-lit :hex-float-lit)
   :decimal-float-lit (-> (p/alt
                            ;; Whole and dot
                            [:decimal-digits \.
                             (p/? :decimal-digits)
                             (p/? :decimal-exponent)]
                            ;; Whole and exp
                            [:decimal-digits :decimal-exponent]
                            ;; Frac and ?exp
                            [\. :decimal-digits (p/? :decimal-exponent)])
                          p/str
                          (p/value parse-float))
   :decimal-exponent  (p/str [(p/one-of "eE") (p/? (p/one-of "+-")) :decimal-digits])
   #_#_:hex-float-lit     (p/let [_ \0 _ #{\x \X}
                              mantissa :hex-mantissa
                              exponent :hex-exponent]
                        (parse-float (str mantissa exponent)))
   #_#_:hex-digits-num    (-> :hex-digits p/str
                          (p/value #(parse-int % 16)))
   #_#_:hex-digits-frac   (p/value (p/str :hex-digits)
                               (fn [hex-str]
                                 (let [width (count hex-str)
                                       ceil  (apply str "1" (repeat width "0"))]
                                   (double (/ (parse-int hex-str 16)
                                              (parse-int ceil    16))))))

   #_#_:hex-mantissa      (p/alt
                        (p/let [_     (p/? \_)
                                whole :hex-digits-num
                                _     \.
                                frac  (p/? :hex-digits-frac)]
                          (str whole "." frac))
                        (p/do (p/? \_) :hex-digits-num)
                        (p/str (p/do \. :hex-digits-frac)))
   ;; Yes, decimal digits in the exponent.
   #_#_:hex-exponent      (p/str [(p/value (p/one-of "pP")
                                       (constantly "e"))
                              (p/? (p/one-of "+-"))
                              :decimal-digits])})

(def go-imaginary
  {:imaginary-lit (p/value [(p/alt (p/value :decimal-digits #(parse-int % 10))
                                   :int-lit
                                   :float-lit)
                            \i]
                           (fn [[i _]] [:complex 0 i]))})
