(ns ml-clj.logistic-test
  (:require [midje.sweet :refer :all]
            [ml-clj.logistic :refer :all]
            [incanter.core :refer :all]
            [incanter.stats :refer :all]
            [incanter.io :refer :all]))

;build some data for validation
(def t-vec (matrix [0 0 0]))
(def t-mat (matrix [[0 0 0] [0 0 0]]))

(def test-data (read-dataset "./test/resources/ex2data1.txt" :header false))
(def X (to-matrix (sel test-data :cols (range 0 2))))
(def y (matrix (sel test-data :cols 2)))


(facts "about the simoid function" 
  (fact
   "the inverse logit (sigmoid) function returns .5 when passed 0"
     (sigmoid 0) => 0.5)
   (fact
    "it should also take in vectors"
    (sigmoid t-vec) => (matrix  [0.5 0.5 0.5]))
   (fact
    "as well as matrices with the same behavior"
    (sigmoid t-mat) => (matrix [[0.5 0.5 0.5]
                                [0.5 0.5 0.5]])))


(facts "about the cost function J we need to minimize"
       (fact
        "there are 100 rows in our training data"
        (nrow X) => 100)
       (fact
        "theta will be a 2x1 matrix given the dim of X"
        (matrix (take (ncol X) (repeat 0))) => (matrix [0 0]))
       (fact
        "given our data the cost function will calc value of 0.693"
        (cost-func X y) => 0.693147180559946))
