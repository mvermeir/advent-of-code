(ns aoc-2020.day-13a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-13a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "939"
                     "7,13,x,x,59,x,31,19"))

(defn up-and-running? [bus-id] (not= bus-id "x"))

(defn parse-loop-times [bus-ids-line]
  (->> (s/split bus-ids-line #",")
       (filter up-and-running?)
       (map #(Integer/parseInt %))))

(defn nearest-departure-after [departure bus-loop-time]
  (let [earliest (->> (iterate #(+ bus-loop-time %) 0)
                      (drop-while #(< % departure))
                      first)]
    (vector earliest bus-loop-time)))

(defn solve [[time-str busses-str]]
  (let [my-earliest-departure (Integer/parseInt time-str)
        loop-times (parse-loop-times busses-str)]
    (->> loop-times
         (map #(nearest-departure-after my-earliest-departure %))
         (apply min-key first)
         ((fn [[earliest-departure bus-id]]
            (* bus-id (- earliest-departure my-earliest-departure)))))))

;; TESTS
(defn test-solution [] (= (solve example-input) 295))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))