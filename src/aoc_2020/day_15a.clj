(ns aoc-2020.day-15a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (list 6 19 0 5 7 13 1))

(def example-input (list 0 3 6))

(defn play-game [turn last-spoken history remaining-turns]
  (let [[x x-1 & xs] (history last-spoken)
        spoken-now (if x-1 (- x x-1) 0)
        new-history (assoc history spoken-now (cons turn (history spoken-now)))]
    (if (not= remaining-turns 0)
      (recur (inc turn) spoken-now new-history (dec remaining-turns))
      spoken-now)))

(defn solve [input]
  (let [current-turn (inc (count input))
        history-at-start (into {} (map-indexed (fn [idx x] [x (list (inc idx))]) input))]
    (play-game current-turn 6 history-at-start (- 2020 current-turn))))

;; TESTS
(defn test-solution [] (= (solve example-input) 436))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))