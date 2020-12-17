(ns aoc-2020.day-16b
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(def input (->> "input-day-16a.txt"
                io/resource
                io/reader
                line-seq))

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
     :your-ticket    (parse-ticket (second your-ticket))
     :rules          (map parse-rule rules)}))

(defn field-matches-rule? [field [_ & ranges]]
  (some (fn [[start end]] (<= start field end)) ranges))

(defn matches-some-rule [field rules]
  (some #(field-matches-rule? field %) rules))

(defn rule-check-fn [rules]
  (fn [ticket]
    (every? #(matches-some-rule % rules) ticket)))


(defn rules-matching-field [rules field]
  (->> (filter #(field-matches-rule? field %) rules)
       (map first)
       (into #{})))

(defn rule-candidates-by-field [rules ticket]
  (->> ticket
       (map #(rules-matching-field rules %))
       vec))

(defn keep-valid-tickets [rules tickets]
  (filter (rule-check-fn rules) tickets))

(defn rule-candidate-sets-per-ticket [rules tickets]
  (map #(rule-candidates-by-field rules %) tickets))

(defn keep-only-those-rules-who-match-all-tickets [rule-candidates-by-field-by-ticket]
  (apply map
    (fn [& rule-candidates-field-x]
      (apply set/intersection rule-candidates-field-x))
    rule-candidates-by-field-by-ticket))

(defn remove-unique-results-from [candidates-per-field unique-results]
  (map #(if (not= (count %) 1)
          (set/difference % unique-results)
          %)
    candidates-per-field))

(defn prune [candidates-per-field]
  (let [unique-results (apply set/union (filter #(= (count %) 1) candidates-per-field))
        pruned (remove-unique-results-from candidates-per-field unique-results)]
    (if (= pruned candidates-per-field)
      (vec (map first candidates-per-field))
      (prune pruned))))

(defn extract-departure-values [ticket ordered-fields]
  (->> (map vector ticket ordered-fields)
       (filter (fn [[_ field]] (s/starts-with? field "departure")))
       (map first)
       (reduce * 1)))

(defn solve [input]
  (let [{nearby-tickets :nearby-tickets
         your-ticket    :your-ticket
         rules          :rules} (parse-input input)]
    (->> nearby-tickets
         (keep-valid-tickets rules)
         (rule-candidate-sets-per-ticket rules)
         keep-only-those-rules-who-match-all-tickets
         prune
         (extract-departure-values your-ticket))))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))