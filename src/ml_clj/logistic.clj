(ns ml-clj.logistic
  (:require [incanter.core :refer [div exp plus minus mult mmult
                                  nrow trans log matrix]]))

(defn sigmoid
  "compute the inverse logit function, large positive numbers should be
close to 1, large negative numbers near 0,
z can be a scalar, vector or matrix.
sanity check: (sigmoid 0) should always evaluate to 0.5"
  [z]
  (div 1 (plus 1 (exp (minus z)))))

(defn cost-func
  "computes the cost function (J) that will be minimized
   inputs: initial-theta X matrix and Y vector"
  [init-vals X y]
  (let
      [m (nrow X)
       z (mmult X init-vals)
       h (sigmoid z)
       J (plus
           (mult (div (minus 1) m) (mmult (trans y) (matrix (log h))))
           (mmult (minus 1.0 (trans y)) (log (minus 1.0 h))))]
    J))


;(defn logistic-regression
 ; "minimize the cost function, input is the cost function and starting values"
  ;[f init-vals]
  ;(minimize cost-func (matrix [0 0] X y (matrix [0 0]))))





