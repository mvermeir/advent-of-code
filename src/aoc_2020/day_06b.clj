(ns aoc-2020.day-06b
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def input (->> "input-day-06a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "abc"
                     ""
                     "a"
                     "b"
                     "c"
                     ""
                     "ab"
                     "ac"
                     ""
                     "a"
                     "a"
                     "a"
                     "a"
                     ""
                     "b"))

(defn end-off-group? [line] (re-matches #"\s*" line))

(defn group-individuals-answers [[groups current] x]
  (if (end-off-group? x)
    (if (seq current)
      [(conj groups current) []]
      [groups []])
    [groups (conj current (into #{} x))]))

(defn parse-group-answers [xs]
  (let [[groups group-in-progress] (reduce group-individuals-answers [[] []] xs)]
    (if (seq group-in-progress)
      (conj groups group-in-progress)
      groups)))

(defn count-group-answers [group]
  (count (apply set/intersection group)))

(defn solve [input]
  (->> (parse-group-answers input)
       (map count-group-answers)
       (reduce + 0)))

;; TESTS
(defn test-solution [] (= (solve example-input) 6))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))