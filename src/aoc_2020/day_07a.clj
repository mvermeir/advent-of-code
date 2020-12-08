(ns aoc-2020.day-07a
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "input-day-07a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "light red bags contain 1 bright white bag, 2 muted yellow bags."
                     "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
                     "bright white bags contain 1 shiny gold bag."
                     "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
                     "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
                     "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
                     "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
                     "faded blue bags contain no other bags."
                     "dotted black bags contain no other bags."))

(defn build-key [bag-part]
  (pop (str/split bag-part #"\s")))

(defn build-content-items [content-part]
  (->> (re-seq #"(\d) (\w+) (\w+)" content-part)
       (reduce (fn [m [_ quantity adj color]]
                 (let [key (vector adj color)]
                   (assoc m key quantity)))
         {})))

(defn parse-rule [line]
  (let [[_ bag-part content-part] (re-matches #"(.+) contain (.+)\." line)]
    [(build-key bag-part) (build-content-items content-part)]))

(defn parse-rules [input]
  (into {} (map parse-rule input)))

(def my-bag-key ["shiny" "gold"])

(defn expand [ks rules]
  (->> (map rules ks)
       (mapcat keys)
       (into #{})))

(defn can-contain-my-bag? [ks remaining-rules]
  (if (ks my-bag-key)
    true
    (let [next-layer-ks (expand ks remaining-rules)
          pruned-remaining (dissoc remaining-rules next-layer-ks)]
      (cond
        (seq next-layer-ks) (recur next-layer-ks pruned-remaining)
        :else false))))

(defn solve [input]
  (let [rules (parse-rules input)]
    (->> (keys rules)
         (filter #(not= % my-bag-key))
         (filter #(can-contain-my-bag? #{%} rules))
         count)))

;; TESTS
(def example-rules (parse-rules example-input))
(defn test-can-contain-bright-white [] (= (can-contain-my-bag? #{["bright" "white"]} example-rules) true))
(defn test-can-contain-dark-orange [] (= (can-contain-my-bag? #{["dark" "orange"]} example-rules) true))
(defn test-can-contain-light-red [] (= (can-contain-my-bag? #{["light" "red"]} example-rules) true))
(defn test-can-contain-faded-blue [] (= (can-contain-my-bag? #{["faded" "blue"]} example-rules) false))

(defn test-solution [] (= (solve example-input) 4))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))