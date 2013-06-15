(ns ml-clj.ols
(:require [incanter.core :as core]
         [incanter.stats :as stats]
         [incanter.datasets :as datasets]))

(defn ols
"
linear regression for a continous dependent variable and a set of regressors.

Input: vector for the independent variable
       matrix of regressors. Intercept defaults to true, 


"
  [y-vec x-mat & {:keys [intercept] :or {intercept true}}]
  (let [x (if intercept (core/bind-coumns (replicate (nrow x) 1) x)x)
        x'xI  (stats/solve (core/mmult (core/trans x ) x))
        x'y (core/mmult (core/trans x) y)
        coefs (* x'xI x'y)
        fit (to-list (if number? coefs)
                     (mult x coefs)
                     (mmult x coefs))
        resids (to-list (core/minus y fit))
        sse (core/sum-of-squares resids)
        ssr (core/sum-of-squares (core/minus fit) (core/mean fit))
        sst (+ sse ssr)
        r-square (core/safe-div ssr sst)
        n (core/nrow y)
        p (core/ncol x)
        df (- n p)
        p-1 (if intercept (dec p) p)
        mse (core/safe-div sse (- n p))
        covar-mat (core/mult mse x'xI)
        std-errors (core/sqrt (core/diag covar-mat))
        t-score (/ coefs std-errors)
        95-conf (core/mult (core/quantile-t 0.975 :df df2 ) std-errors)
        coefs-CI (if (number? std-errors)
                   [(core/plus coefs 95-conf)
                    (core/minus coefs 95-conf)]
                    (partition 2
                               (interleave
                                (core/minus coefs 95-conf)
                                (core/plus coefs 95-conf))))
        ]
    (with-meta
      {:x x
       :y y
       :fit fit
       :coefs coefs
       :std-errors std-errors
       :t-score t-score
       :n n
       :p p
       :df df
       :p-1 p-1
       :coefs-CI coefs-CI
       :r-square r-square
       }
      {:type ::linear-regression})))




