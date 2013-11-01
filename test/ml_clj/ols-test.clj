(ns ml-clj.ols-test
  (:require [midje.sweet :refer :all]
            [ml-clj.ols :refer :all]
            [incanter.core :refer :all]
             [incanter.stats :refer :all]
             [incanter.datasets :refer :all]))

(def iris (to-matrix (get-dataset :iris) :dummies true))
(def y (sel iris :cols 0))
(def x (sel iris :cols (range 1 6)))
(def iris-lm (linear-model y x))

(def incanter-lm (linear-model y x))
(def ian-lm (ols y x))

;refactor for midje unit test, compare my map to incanter
(fact
 "the vals from ian's regression are the same as incanter"
 (vals incanter-lm) => (vals ian-lm))

