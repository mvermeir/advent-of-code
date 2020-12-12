(ns aoc-2020.day-10b
  (:require [clojure.java.io :as io]))

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

(defn replace-last [v x]
  (conj (pop v) x))

(defn group-by-streak [streaks x]
  (let [current-streak (last streaks)
        x-1            (last current-streak)
        diff           (- x x-1)]
    (if (= diff 1)
      (replace-last streaks (conj current-streak x))
      (conj streaks [x]))))

(defn chop-into-continuous-streaks [xs]
  (reduce group-by-streak [[0]] xs))

(defn tribonacci [x]
  (cond
    (= x 1) 1
    (= x 2) 1
    (= x 3) 2
    :else (+ (tribonacci (- x 3)) (tribonacci (- x 2)) (tribonacci (- x 1)))))

(defn nb-of-alternate-paths [xs]
  (tribonacci (count xs)))

(defn solve [input]
  (->> (map #(Integer/parseInt %) input)
       (sort <)
       chop-into-continuous-streaks
       (map nb-of-alternate-paths)
       (reduce * 1)))

;; TESTS
(defn test-solution [] (= (solve example-input) 8))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))