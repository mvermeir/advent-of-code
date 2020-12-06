(ns aoc-2020.day-05a
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-05a.txt"
                io/resource
                io/reader
                line-seq))

(defn next-split [[start end] direction]
  (cond
    (or (= \F direction) (= \L direction))
    [start (-> (+ start end) dec (/,,, 2))]

    (or (= \B direction) (= \R direction))
    [(-> (+ start end) inc (/,,, 2)) end]))

(defn determine-index [binary-coordinate]
  (let [range-of-one-left (reduce
                            (fn [range direction]
                              (next-split range direction))
                            [0 (dec (reduce * 1 (repeat (count binary-coordinate) 2)))]
                            (seq binary-coordinate))]
    (first range-of-one-left)))

(defn determine-regular-coordinates [binary-coordinate]
  (let [[_ row-coord col-coord] (re-matches #"([BF]+)([LR]+)" binary-coordinate)]
    [(determine-index row-coord) (determine-index col-coord)]))

(defn calculate-id [[row column]]
  (+ (* row 8) column))

(defn determine-seat-id [binary-coordinate]
  (->> binary-coordinate
       determine-regular-coordinates
       calculate-id))

(defn solve [input]
  (->> input
       (map determine-seat-id)
       (apply max)))

;; TESTS
(defn test-seat-id [] (= (determine-seat-id "FBFBBFFRLR") 357))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))