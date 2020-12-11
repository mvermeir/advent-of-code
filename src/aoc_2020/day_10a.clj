(ns aoc-2020.day-10a
  (:require [clojure.java.io :as io]
            ))

(def input (->> "input-day-10a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "16"
                     "10"
                     "15"
                     "5"
                     "1"
                     "11"
                     "7"
                     "19"
                     "6"
                     "12"
                     "4"))

;; precondition the list always has at least 1 element
(defn add-diff-to-tally [[diffs-of-1 diffs-of-3 :as tally] x-1 x]
  (case (- x x-1)
    1 [(inc diffs-of-1) diffs-of-3]
    3 [diffs-of-1 (inc diffs-of-3)]
    tally))

(defn tally-differences
  ([xs]
   (tally-differences [0 0] 0 (first xs) (rest xs)))
  ([tally x-1 x xs]
   (let [[diffs-of-1 diffs-of-3 :as new-tally] (add-diff-to-tally tally x-1 x)]
     (if (seq xs)
       (recur new-tally x (first xs) (rest xs))
       [diffs-of-1 (inc diffs-of-3)]))))

(defn solve [input]
  (->> (map #(Integer/parseInt %) input)
       (sort <)
       tally-differences
       (reduce *)))

;; TESTS
(defn test-solution [] (= (solve example-input) 35))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))