(ns ml-clj.naive-bayes)

(def test-docs [["my" "dog" "has" "flea" "problems" "please" "help"]
		   ["maybe" "not" "take" "him" "to" "dog" "park" "stupid"]
		   ["my" "dalmation" "is" "so" "cute" "I" "love" "him"]
		   ["stop" "posting" "stupid" "worthless" "garbage" ]
		   ["mr" "licks" "ate" "my" "steak" "how" "to" "stop" "him"]
		   ["quit" "buying" "worthless" "dog" "food" "stupid"]])

(def doc-class [0 1 0 1 0 1])

(defn normalize-doc [documents]
  "from a group of documents find all distinct strings"
  (distinct (flatten documents)))

;how many times does each word from vocab list appear in each doc?

(def ham (atom {}))
(def spam (atom {}))

(def spam [doc m]
  "trains spam filter using words in the doc"
  (train-doc! doc spam))

(def ham [doc m]
  "trains ham filter using words in the doc"
  (train-doc! doc ham))

(defn train-word! [goodbad word]
  (swap! goodbad update-in [word]
                     (fn [old] (inc (or old 0)))))

(defn train-doc! [goodbad doc]
  (reduce train-word! {} (normalize-doc doc)))

(defn calculate-frequencies [words]
 "convert list of words to a word-frequency hash"
 (reduce (fn [words word] (assoc words word (inc (get words word 0))))
  {}
  words))

;TODO this currently throws npe when word not found
(defn score-word [word good bad]
  "score the prob that word is spam"
  (/ (or (@bad word) 0) (+ (or (@bad word) 0) (or (@good word) 0))))

(defn score-document [doc]
  "scores the probability that a document is spam"
  (let [
        words (normalize-document doc)
        w-probs (for [w words]
                  (score-word w))]
    (/ (reduce + w-probs) (length w-probs))))

