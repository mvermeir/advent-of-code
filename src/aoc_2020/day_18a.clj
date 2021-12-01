(ns aoc-2020.day-18a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-18a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "2 * 3 + (4 * 5)"
                     "5 + (8 * 3 + 9 + 3 * 4 * 3)"
                     "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
                     "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

(def my-map {

             })

(defn solve [input]
  )

;; TESTS
(defn test-solution [] (= (solve example-input) 112))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))