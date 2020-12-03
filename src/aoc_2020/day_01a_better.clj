(ns aoc-2020.day-01a-better
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-01a.txt"
                 io/resource
                 io/reader
                 line-seq
                 (map read-string)))

(defn combine [x xs]
  (map #(vector x %) xs))

(defn self-combine [[x & xs]]
  (if (seq xs)
    (concat (combine x xs) (self-combine xs))
    xs))

(defn set->set-and-total [xs]
  {:total (reduce + xs) :set xs})

(defn solve-a [xs]
  (->> (self-combine xs)
       (map set->set-and-total)
       (filter #(= 2020 (% :total)))
       (map :set)
       first
       (reduce *)))

;; TESTS
(defn test-example [] (= (solve-a (list 1721 979 366 299 675 1456)) 514579))
(defn test-set-with-1010 []
  (= (solve-a
       (list 1010 1721 979 366 299 675 1456))
    514579))
(defn test-set-with-1010-twice []
  (= (solve-a
       (list 1010 1721 979 366 300 1010 1456))
    1020100))

(defn -main
  "Main function"
  [& args]
  (println (solve-a input)))