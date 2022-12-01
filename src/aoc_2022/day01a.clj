(ns aoc-2022.day01a
  (:require [util :as util]
            [clojure.string :as str]))

(def example-input (list "1000"
                     "2000"
                     "3000"
                     ""
                     "4000"
                     ""
                     "5000"
                     "6000"
                     ""
                     "7000"
                     "8000"
                     "9000"
                     ""
                     "10000"))

;;(def input (util/file->seq "2021/d01a.txt"))

(defn aggregate-elves-and-select-max-calories [input]
  (->> (util/split-using str/blank? input)
       (map (fn [xs] (map util/to-int xs)))
       (map (fn [xs] (reduce + 0 xs)))
       (apply max)))

(comment
  ;; verify example input
  (= 24000 (aggregate-elves-and-select-max-calories example-input))
  ,,,)

(defn -main
  "Main function"
  []
  (println (aggregate-elves-and-select-max-calories (util/file->seq "2022/d01a.txt"))))