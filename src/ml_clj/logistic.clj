(ns ml-clj.logistic
  (:require [incanter.core :refer :all]
            [incanter.optimize :refer :all
             ]))

(defn sigmoid
  "compute the inverse logit function, large positive numbers should be
close to 1, large negative numbers near 0,
z can be a scalar, vector or matrix.
sanity check: (sigmoid 0) should always evaluate to 0.5"
  [z]
  (div 1 (plus 1 (exp (minus z)))))

(defn cost-func
  "computes the cost function (J) that will be minimized
   inputs:params theta X matrix and Y vector"
  [X y]
  (let
      [m (nrow X)
       init-vals (matrix (take (ncol X) (repeat 0)))
       z (mmult X init-vals)
       h (sigmoid z)
       f-half (mult (matrix (map - y)) (log (sigmoid (mmult X init-vals))))
       s-half (mult (minus 1 y) (log (minus 1 (sigmoid (mmult X init-vals)))))
       sub-tmp (minus f-half s-half)
       J (mmult (/ 1 m) (reduce + sub-tmp))]
    J))
