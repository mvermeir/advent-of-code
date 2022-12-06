(ns aoc-2022.day03
  (:require [util :as util]
            [clojure.string :as str]
            [clojure.set :as s]))

(def example-input (list "vJrwpWtwJgWrhcsFMMfFFhFp"
                     "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                     "PmmdzqPrVvPwwTWBwg"
                     "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                     "ttgJtRGJQctTZtZT"
                     "CrZsJsPPZsGzwwsLwLmpwMDw"))

(def priorities (->> (map-indexed (fn [idx it] [it (inc idx)]) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
                     (into {})))

(defn find-badly-packed-item-types [rucksack]
  (let [item-count-per-compartment (/ (count rucksack) 2)
        first-compartment (take item-count-per-compartment rucksack)
        second-compartment (drop item-count-per-compartment rucksack)]
    (s/intersection (set first-compartment) (set second-compartment))))

(defn sum-priorities [badly-backed-item-types]
  (->> (map priorities badly-backed-item-types)
       (reduce + 0)))
(defn sum-of-priorities-of-badly-packed-item-types [input]
  (->> input
       (map find-badly-packed-item-types)
       (map sum-priorities)
       (reduce + 0)))

(comment
  (= #{\p} (find-badly-packed-item-types "vJrwpWtwJgWrhcsFMMfFFhFp"))
  (= 17 (sum-priorities #{\p \a}))

  ;; verify example input
  (= 157 (sum-of-priorities-of-badly-packed-item-types example-input))

  ,,,)

(defn -main
  "Main function"
  []
  (let [input (util/file->seq "2022/d03.txt")]
    (sum-of-priorities-of-badly-packed-item-types input)))