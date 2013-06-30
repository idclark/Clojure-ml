(ns ml-clj.logistic-test
  (:use midje.sweet
        ml-clj.logistic
        ))

(fact
 "the inverse logit (sigmoid) function returns .5 when passed 0"
 (sigmoid 0) => 0.5)


