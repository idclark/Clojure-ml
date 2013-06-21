(ns ml-clj.core-test
  (:use clojure.test
        ml-clj.ols
        incanter.core
        incanter.stats
        incanter.datasets))

(def iris (to-matrix (get-dataset :iris) :dummies true))
  (def y (sel iris :cols 0))
  (def x (sel iris :cols (range 1 6)))
  (def iris-lm (linear-model y x))

(def incanter-lm (linear-model y x))
(def ian-lm (ols y x))

(deftest coef-test
  (testing "coefs"
    (= (:coefs incanter-lm) (:coefs ian-lm))))

(deftest std-error-test
  (testing "std-errors"
    (= (:std-errors incanter-lm) (:std-errors ian-lm))))

(deftest r-square-test
  (testing "r-square"
    (= (:r-square incanter-lm) (:r-square ian-lm))))
