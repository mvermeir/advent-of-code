(ns aoc-2020.day-06a
  (:require [clojure.java.io :as io]))

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
    [groups (conj current x)]))

(defn parse-group-answers [xs]
  (let [[groups group-in-progress] (reduce group-individuals-answers [[] []] xs)]
    (if (seq group-in-progress)
      (conj groups group-in-progress)
      groups)))

(defn count-group-answers [group]
  (->> (mapcat seq group)
       (into #{})
       count))

(defn solve [input]
  (->> (parse-group-answers input)
       (map count-group-answers)
       (reduce + 0)))



;; TESTS
(defn test-solution [] (= (solve example-input) 11))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))