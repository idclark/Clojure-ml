; collection of utility functions, random stuff i dont know where to put
(ns ml-clj.utils
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]))
;lazily read in csv
(defn lazy-read
  [csv-file]
  (with-open [infile
              (io/reader csv-file)]
    (frequencies (map #(nth % 2)
                      (csv/read-csv in-file)))))

;sample from a dataset
(defn sample-percent
  [k coll]
  (filter (fn [_]
            (<= (rand) k)) coll))





