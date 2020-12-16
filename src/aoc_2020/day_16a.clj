(ns aoc-2020.day-16a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-16a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "class: 1-3 or 5-7"
                     "row: 6-11 or 33-44"
                     "seat: 13-40 or 45-50"
                     ""
                     "your ticket:"
                     "7,1,14"
                     ""
                     "nearby tickets:"
                     "7,3,47"
                     "40,4,50"
                     "55,2,20"
                     "38,6,12"))

(defn split-into-sections [lines]
  (reduce
    (fn [sections l]
      (cond
        (empty? sections)
        (cons [l] sections)

        (re-matches #"\s*" l)
        (cons [] sections)

        :else
        (let [current-section (first sections)]
          (cons (conj current-section l) (rest sections)))))
    '() lines))

(defn parse-ticket [ticket-str]
  (->> (re-seq #"(\d+)" ticket-str)
       (filter seq)
       (map (fn [[_ nb]] (Integer/parseInt nb)))
       vec))

(defn parse-range [range-str]
  (->> (re-seq #"(?:(\d+)-(\d+))" range-str)
       (map (fn [[_ start-str end-str]]
              [(Integer/parseInt start-str) (Integer/parseInt end-str)]))))

(defn parse-rule [line]
  (let [[field ranges] (s/split line #"\:")]
    (into [field] (parse-range ranges))))

(defn parse-input [lines]
  (let [[nearby-tickets your-ticket rules] (split-into-sections lines)]
    {:nearby-tickets (->> (map parse-ticket nearby-tickets) (filter seq))
     ;;:your-ticket    (parse-ticket your-ticket)
     :rules          (map parse-rule rules)}))

(defn matches-some-rule [field rules]
  (some
    (fn [[_ & ranges]]
      (some (fn [[start end]] (<= start field end)) ranges))
    rules))

(defn extract-error-values-fn [rules]
  (fn [ticket]
    (filter #(not (matches-some-rule % rules)) ticket)))

(defn solve [input]
  (let [{nearby-tickets :nearby-tickets
         rules :rules} (parse-input input)]
    (->> nearby-tickets
         (map (extract-error-values-fn rules))
         (apply concat)
         (reduce +))))

;; TESTS
(defn test-solution [] (= (solve example-input) 71))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))