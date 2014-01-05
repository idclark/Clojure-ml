(ns naive-bayes-test
  (:require [midje.sweet :refer :all]))

(def test-docs [["my" "dog" "has" "flea" "problems" "please" "help"]
		   ["maybe" "not" "take" "him" "to" "dog" "park" "stupid"]
		   ["my" "dalmation" "is" "so" "cute" "I" "love" "him"]
		   ["stop" "posting" "stupid" "worthless" "garbage" ]
		   ["mr" "licks" "ate" "my" "steak" "how" "to" "stop" "him"]
		   ["quit" "buying" "worthless" "dog" "food" "stupid"]])

(def ham (atom {}))
(def spam (atom {}))
(def doc (first test-docs))

(facts "about naive bayes spam filter"
       (fact
        "train individual words"
        (train-word! ham "cat") => {"cat" 1})
       (fact
        "train documents"
        (train-doc! ham doc) => {"help" 1, "please" 1, "problems" 1, "flea" 1, "has" 1, "dog" 1, "my" 1, "cat" 2})
       )


