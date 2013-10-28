(ns ml-clj.logistic-test
  (:use [midje sweet]
            [ml-clj logistic]
            [incanter core stats]))

(def t-vec (matrix [0 0 0]))
(def t-mat (matrix [[0 0 0] [0 0 0]]))

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



