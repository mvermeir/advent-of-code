(ns aoc-2020.day-01b
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-01a.txt"
                 io/resource
                 io/reader
                 line-seq
                 (map read-string)))

(defn generate-combinations [xs]
  (let [indexed (map-indexed vector xs)]
    (for [[i x] indexed
          [j y] indexed :when (> j i)
          [k z] indexed :when (> k j)]
      [x y z])))

(defn set->set-and-total [xs]
  {:total (reduce + xs) :set xs})

(defn solve [xs]
  (->> (generate-combinations xs)
       (map set->set-and-total)
       (filter #(= 2020 (% :total)))
       (map :set)
       first
       (reduce *)))

;; TESTS
(defn test-example [] (= (solve (list 1721 979 366 299 675 1456)) 241861950))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))