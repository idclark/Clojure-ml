(ns ml-clj.ols
(use '(incanter core stats))
(require '[clatrix.core :as M]))

(defn ols
  "
linear regression for a continous dependent variable

"
  [y-vec x-mat & {:keys intercept no-intercept}]
  (let [y (M/matrix y-vec)
        x (M/matrix x-mat)
        x'xI (M/i (M/* (M/t x) x))
        x'y (M/* (M/t x) x)
        coefs (to-list (if (and (number? x'xI) (number? x'y))
                (M/* x'xI x'y)))
        fit (to-list (if (number? coefs)
                       (M/* x coefs )))
        resids (to-list (- y fit))
        ssq (to-list
             (/
             (M/* (M/t resids) resids)
             (- (nrows coefs) (ncols x))))
        varcovar (clojure.core/*
                  ssq
                  (M/i (M/* (M/t x) x)))
        std-error (to-list(M/sqrt (M/diag varcovar)))
                             
                  ]))

;convert y col to a vector
;convert x to matrix
;bind 1s length x to x for intercept
; x'x-1 x'y
;SEs
; t score
;
(def iris (to-matrix (get-dataset :iris) :dummies true))
    (def y (sel iris :cols 0))
    (def x (sel iris :cols (range 1 6)))
    (def iris-lm (linear-model y x)) ; with intercept term
