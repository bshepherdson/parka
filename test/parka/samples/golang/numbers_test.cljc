(ns parka.samples.golang.numbers-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [parka.api :as p]
   [parka.samples.golang.test-util :as tu]))

(deftest int-literals-test
  (are [exp input] (= exp (tu/parse-from :int-lit input))
       42              "42"
       42              "4_2"
       384             "0600"
       384             "0_600"
       384             "0o600"
       384             "0O600" ;; That's a capital O
       0xbadface       "0xBadFace"
       0xbadface       "0xBad_Face"
       0x677a2fcc40c6  "0x_67_7a_2f_cc_40_c6")

  (testing "failures"
    (are [input] (= true (-> (tu/parse-from :int-lit input)
                             :error :parka/parse-error))
         "_42"
         "42_"
         "4__2"
         "0_xBadFace")))

(comment
  ;; START HERE: This case is busted. This is what happens to the EOF check when
  ;; it reaches the top level.
  (tu/parse-from :int-lit "42_")

  )

(deftest float-literals-test
  (are [exp input] (= exp (tu/parse-from :float-lit input))
       0.           "0."
       72.40        "72.40"
       72.40        "072.40"
       2.71828      "2.71828"
       1.0          "1.e+0"
       6.67428e-11  "6.67428e-11"
       1E6          "1E6"
       0.25         ".25"
       0.12345E+5   ".12345E+5"
       15.0         "1_5."
       15.0         "0.15e+0_2"

       ;; TODO: Resurrect hex floats? They're really clumsy to parse from other
       ;; platforms since nearly no one supports them.
       ;0.25         "0x1p-2"
       ;2048.0       "0x2.p10"
       ;1.9375       "0x1.Fp+0"
       ;0.5          "0X.8p-0"
       ;0.1249847412109375 "0X_1FFFP-16"
       #_(- 0x15e 2)  #_"0x15e-2" #_(integer subtraction))

  #_(testing "failures"
    (are [input] (= true (-> (tu/parse-from :int-lit input)
                             :error :parka/parse-error))
         "_42"
         "42_"
         "4__2"
         "0_xBadFace")))

(deftest imaginary-literals-test
  (are [exp input] (= [:complex 0 exp] (tu/parse-from :imaginary-lit input))
       0     "0i"
       123   "0123i"))
