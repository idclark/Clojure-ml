(ns ml-clj.ols
(:require [incanter.core :as core]
         [incanter.stats :as stats]
         [incanter.datasets :as datasets]))

(defn ols
"
linear regression for a continous dependent variable and a set of regressors.

Input: vector for the independent variable
       matrix of regressors. Intercept defaults to true.



"
  [y-vec x-mat & {:keys [intercept] :or {intercept true}}]
  (let [x (if intercept (core/bind-columns (replicate (core/nrow x-mat) 1) x-mat) x-mat)
        x'xI  (core/solve (core/mmult (core/trans x ) x))
        x'y (core/mmult (core/trans x) y-vec)
        coefs (core/mmult x'xI x'y)
        fit (core/to-list (if number? coefs)
                     (core/mult x coefs)
                     (core/mmult x coefs))
        resids (core/to-list (core/minus y-vec fit))
        sse (core/sum-of-squares resids)
        ssr (core/sum-of-squares (core/minus fit (stats/mean fit))
        sst (+ sse ssr)
        r-square (core/safe-div ssr sst)
        n (core/nrow y-vec)
        p (core/ncol x)
        df (- n p)
        p-1 (if intercept (dec p) p)
        mse (core/safe-div sse (- n p))
        covar-mat (core/mult mse x'xI)
        std-errors (core/sqrt (core/diag covar-mat))
        t-score (/ coefs std-errors)
        conf-95 (core/mult (stats/quantile-t 0.975 :df df ) std-errors)
        coefs-CI (if (number? std-errors)
                   [(core/plus coefs conf-95)
                    (core/minus coefs conf-95)]
                    (partition 2
                               (interleave
                                (core/minus coefs conf-95)
                                (core/plus coefs conf-95))))
        ]
    (with-meta
      {:x x
       :y y-vec
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




