(ns ml-clj.ols
(:require [clatrix.core :as C]
         [incanter.core :as core]
         [incanter.stats :as stats]
         [incanter.datasets :as datasets]))

(defn ols
  "
linear regression for a continous dependent variable

"
  [y-vec x-mat & {:keys [intercept] :or {intercept true}}]
  (let [y (C/matrix y-vec)
        x (C/matrix x-mat)
        x'xI (C/i (C/* (C/t x) x))
        x'y (C/* (C/t x) x)
        coefs (core/to-list (if (and (number? x'xI) (number? x'y))
                (C/* x'xI x'y)))
        fit (core/to-list (if (number? coefs)
                       (C/* x coefs )))
        resids (core/to-list (- y fit))
        ssq (core/to-list
             (/
             (C/* (C/t resids) resids)
             (- (C/nrows coefs) (C/ncols x))))
        varcovar (*
                  ssq
                  (C/i (C/* (C/t x) x)))
        std-error (core/to-list (C/sqrt (C/diag varcovar)))
        t-score (core/to-list(/ coefs std-error))
        ]))
  ;(with-meta
   ; {:coefs coefs
    ;:std-errors std-error
    ;:t-score t-score}))
    
                             

;convert y col to a vector
;convert x to matrix
;bind 1s length x to x for intercept
; x'x-1 x'y
;SEs
; t score
;
;(def iris (to-matrix (get-dataset :iris) :dummies true))
 ;   (def y (sel iris :cols 0))
  ;  (def x (sel iris :cols (range 1 6)))
   ; (def iris-lm (linear-model y x)) ; with intercept term
