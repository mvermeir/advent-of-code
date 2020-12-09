(ns aoc-2020.day-07b
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "input-day-07a.txt"
                io/resource
                io/reader
                line-seq))

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

(defn explode-bag-content [content]
  (mapcat (fn [[k quantity]]
            (repeat (Integer/parseInt quantity) k)) content))

(defn expand [ks rules]
  (->> (map rules ks)
       (mapcat explode-bag-content)))

(defn compose [ks total rules]
  (let [new-total (+ total (count ks))
        next-layer-ks (expand ks rules)]
    (if (seq next-layer-ks)
      (recur next-layer-ks new-total rules)
      new-total)))

(defn solve [input]
  (let [rules (parse-rules input)]
    (dec (compose [my-bag-key] 0 rules))))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))