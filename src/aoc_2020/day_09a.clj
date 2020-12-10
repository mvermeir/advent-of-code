(ns aoc-2020.day-09a
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "input-day-09a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "35"
                     "20"
                     "15"
                     "25"
                     "47"
                     "40"
                     "62"
                     "55"
                     "65"
                     "95"
                     "102"
                     "117"
                     "150"
                     "182"
                     "127"
                     "219"
                     "299"
                     "277"
                     "309"
                     "576"))

(defn group-number-with-preamble [preamble-size raw-list]
  (->> (interleave (partition preamble-size 1 raw-list) (drop preamble-size raw-list))
       (partition 2)
       (map vec)
       (map (fn [[preamble-as-list nb]] [(vec preamble-as-list) nb]))))

(defn generate-all-possible-sums [xs]
  (->> (for [x xs y xs :when (< x y)]
         (+ x y))
       (into #{})))

(defn can-be-encoded-by-preamble? [[preamble x]]
  ((generate-all-possible-sums preamble) x))

(defn cannot-be-encoded-by-preamble? [x]
  (not (can-be-encoded-by-preamble? x)))

(defn solve [input preamble-size]
  (->> (map read-string input)
       (group-number-with-preamble preamble-size)
       (filter cannot-be-encoded-by-preamble?)
       (map second)
       first))

;; TESTS
(defn test-solution [] (= (solve example-input 5) 127))

(defn -main
  "Main function"
  [& args]
  (println (solve input 25)))