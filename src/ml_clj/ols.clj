(ns ml-clj.ols
(:use [incanter.core :only ( abs plus minus div mult mmult to-list bind-columns
                               sqrt diag trans  ncol
                              nrow matrix length  sum sum-of-squares sel solve
                              bind-rows safe-div)]
      [incanter.stats :only (mean quantile-t cdf-f cdf-t)]))

(defn ols
"
linear regression for a continous dependent variable and a set of regressors.

Input: vector for the independent variable
       matrix of regressors. Intercept defaults to true.

returns a map containing:
coefs: the coefficient values
std-errors: the standard errors of the coefficients
resid: the residuals
sse: sum of squared errors
ssr: regression sum of squares
r-square: coefficient of fit (range: 0 - 1)
t-test: coefs / std-errors
n: number of oberservation
p: number of columns in regressor matrix
coefs-ci: 95 percentile confidence intervals for each coeficient value

"
([y x & {:keys [intercept] :or {intercept true}}]
    (let [_x (if intercept (bind-columns (replicate (nrow x) 1) x) x)
          xtx (mmult (trans _x) _x)
          xtxi (if (number? xtx) (/ 1 xtx) (solve xtx))
          xty (mmult (trans _x) y)
          coefs (if (and (number? xtxi) (number? xty))
                  (* xtxi xty)
                  (to-list (if (or (number? xtxi) (number? xty))
                    (mult xtxi xty)
                    (mmult xtxi xty))))
          fitted (to-list (if (number? coefs)
                  (mult _x coefs)
                  (mmult _x coefs)))
          resid (to-list (minus y fitted))
          sse (sum-of-squares resid)
          ssr (sum-of-squares (minus fitted (mean fitted)))
          sst (+ sse ssr)
          r-square (safe-div ssr sst)
          n (nrow y)
          p (ncol _x)
          p-1 (if intercept (dec p) p)
          adj-r-square (- 1 (* (- 1 r-square) (/ (dec 1) (- n p 1))))
          mse (safe-div sse (- n p))
          msr (safe-div ssr p-1)
          f-stat (safe-div msr mse)
          df1 p-1
          df2 (- n p)
          f-prob (cdf-f f-stat :df1 df1 :df2 df2 :lower-tail? false)
          coef-var (mult mse xtxi)
          std-errors (sqrt (diag coef-var))
          t-tests (div coefs std-errors)
          t-probs (mult 2 (cdf-t (abs t-tests) :df df2 :lower-tail? false))
          t-95 (mult (quantile-t 0.975 :df df2) std-errors)
          coefs-ci (if (number? std-errors)
                       [(plus coefs t-95)
                        (minus coefs t-95)]
                       (partition 2
                         (interleave
                           (minus coefs t-95)
                           (plus coefs t-95))))
         ]
      (with-meta
        {:x _x
         :y y
         :fitted fitted
         :design-matrix _x
         :coefs coefs
         :t-tests t-tests
         :t-probs t-probs
         :coefs-ci coefs-ci
         :residuals resid
         :std-errors std-errors
         :sse sse
         :ssr ssr
         :sst sst
         :mse mse
         :msr msr
         :f-stat f-stat
         :f-prob f-prob
         :df [df1 df2]
         :coef-var coef-var
         :r-square r-square
         :adj-r-square adj-r-square
        }
        {:type ::linear-model}))))
 




