(ns aoc-2020.day-09b
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

(defn find-weakness [nbs preamble-size]
  (->> nbs
       (group-number-with-preamble preamble-size)
       (filter cannot-be-encoded-by-preamble?)
       (map second)
       first))

(defn in-groups-of [n xs]
  (->> (partition n 1 xs)
       (map #(vector (reduce + 0 %) %))))

(defn search-for-encryption-weakness [nbs nb window]
  (->> (in-groups-of window nbs)
       (filter #(= (first %) nb))
       (map second)
       first))

(defn sum-largest-2 [xs]
  (let [sorted-vector (->> (sort xs) (vec))]
    (+ (first sorted-vector) (last sorted-vector))))

(defn solve [input preamble-size]
  (let [nbs (map read-string input)
        weakness (find-weakness nbs preamble-size)]
    (->> (iterate inc 2)
         (map #(search-for-encryption-weakness nbs weakness %))
         (drop-while empty?)
         (take 1)
         first
         (sum-largest-2 ))))

;; TESTS
(defn test-solution [] (= (solve example-input 5) 127))

(defn -main
  "Main function"
  [& args]
  (println (solve input 25)))