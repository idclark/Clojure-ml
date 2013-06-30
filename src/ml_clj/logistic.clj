(ns ml-clj.logistic
  (:use [incanter.core :only (div exp plus minus mult mmult
                                  nrow trans log)]))

(defn sigmoid
  "compute the inverse logit function, large positive numbers should be
close to 1, large negative numbers near 0,
z can be a scalar, vector or matrix.
sanity check: (sigmoid 0) should always evaluate to 0.5"
  [z]
  (div 1 (plus 1 (exp (minus z)))))

(defn compute-cost
  "computes the cost function that will be minimized
   inputs: initial-theta X matrix and Y vector"
  [init-vals X y]
  (let
      [m (nrow X)
       z (mmult X init-vals)
       h (sigmoid z)
       part-one (mult (div (minus 1) m) (mmult (trans y) log h))
       part-two (mmult (minus 1.0 (trans y)) (log (minus 1.0 h)))
       J (plus part-one part-two)
       ]
    J))


(defn logistic-regression
  [stuff]
  (println stuff))

;define sigmoid function,
; initial guess
; incanter maximize to find best values


