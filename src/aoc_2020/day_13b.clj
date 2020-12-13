(ns aoc-2020.day-13b
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-13a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "939"
                     "7,13,x,x,59,x,31,19"))

(defn up-and-running? [[_ bus-id]] (not= bus-id "x"))

(defn parse-offset-and-id [bus-ids-line]
  (->> (s/split bus-ids-line #",")
       (map-indexed (fn [offset-from-t id-str] [offset-from-t id-str]))
       (filter up-and-running?)
       (map (fn [[offset id-str]]
              [offset (Integer/parseInt id-str)]))))

;; (mod (+ t offset) id)
(defn nearest-departure-after [departure bus-loop-time]
  (let [earliest (->> (iterate #(+ bus-loop-time %) 0)
                      (drop-while #(< % departure))
                      first)]
    (vector earliest bus-loop-time)))

(defn generate-departures-for [[_ bus-id]]
  (iterate #(+ bus-id %) 0))

(defn timings-match-up? [offset-and-ids]
  (fn [time]
    (println "checking " time)
    (every? (fn [[offset id]] (= 0 (mod (+ time offset) id))) offset-and-ids)))

(defn solve [[_ bus-ids-str]]
  (let [[pair-for-first-bus & pairs-for-other-busses] (parse-offset-and-id bus-ids-str)]
    (->> (generate-departures-for pair-for-first-bus)
         (filter (timings-match-up? pairs-for-other-busses))
         first)))

;; TESTS
(defn test-solution [] (= (solve example-input) 1068781))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))