(ns aoc-2024.day01
  (:require [util :as util]
            [clojure.string :as str]
            [clojure.test :as test]))

(def example-input '("3 4"
                     "4 3"
                     "2 5"
                     "1 3"
                     "3 9"
                     "3 3"))


(defn distances [lines]
  (let [[as bs] (reduce (fn [[as bs] x]
            (let [[a b] (str/split x #"\s+")]
              [(conj as (util/to-int a)) (conj bs (util/to-int b))]))
          ['() '()]
          lines)]
    
    (map (fn [a b] (util/abs (- a b))) (sort as) (sort bs))))

(defn total-distance [lines]
  (->> lines
       distances
       (reduce + 0)))

(comment
  (total-distance example-input)
  ,,,)

(defn -main
  "Main function"
  []
  (let [input (util/file->seq "2024/d1p1.txt")]
    (total-distance input)))
