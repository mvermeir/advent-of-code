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

(defn solve-for-time-and-sort [pairs]
  (->> (map (fn [[offset-from-t id]]
              (let [remainder-of-div-t-by-id (mod (- id offset-from-t) id)]
                [remainder-of-div-t-by-id id]))
         pairs)
       (sort (fn [[_ modulo-1] [_ modulo-2]]
               (compare modulo-2 modulo-1)))))

;; This function progressively "sieves" more and more candidates by adding in more of the modulo conditions
;; until one candidate is found for which all the remainders are as expected
(defn sieve
  ([pairs]
   (let [[remainder id] (first pairs)]
     (sieve remainder id 0 (rest pairs))))

  ([time-so-far modulo m pairs]
   (if (seq pairs)
     (let [new-candidate (+ time-so-far (* m modulo))
           [expected-remainder current-id] (first pairs)]
       (if (= (mod new-candidate current-id) expected-remainder)
         ;; sieve on the next pair so it can be integrated
         (recur new-candidate (* modulo current-id) 0 (rest pairs))
         ;; current modulo condition not yet met -> try again
         (recur time-so-far modulo (inc m) pairs)))
     ;; all modulo conditions are met
     time-so-far)))

(defn solve [[_ bus-ids-str]]
  (->> (parse-offset-and-id bus-ids-str)
       solve-for-time-and-sort
       sieve))

;; TESTS
(defn test-solution [] (= (solve example-input) 1068781))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))