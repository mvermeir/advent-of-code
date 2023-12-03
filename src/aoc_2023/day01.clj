(ns aoc-2023.day01
  (:require [util :as util]
            [clojure.test :as test]))

(def example-input '("1abc2"
                    "pqr3stu8ewx"
                    "a1b2c3d4e5f"
                    "treb7uceet"))
(defn digit? [char]
  (java.lang.Character/isDigit char))

(defn grab-first-number-in-character-seq [chars]
  (->> chars
       (drop-while (fn [x] (not (digit? x))))
       (take-while (fn [x] (digit? x)))))

(defn parse-calibration [line]
  (let [first-digit (->> line
                         (grab-first-number-in-character-seq)
                         (apply str))
        last-digit (->> line
                        reverse
                        grab-first-number-in-character-seq
                        reverse
                        (apply str))]
    (util/to-int (str first-digit last-digit))))

(test/is
  (= (parse-calibration "1abc2") 12))


(defn read-calibration-file [file-name]
  (->> file-name
       util/file->seq
       (map parse-calibration)
       (reduce +)))

(comment
  ;; verify example input
  (+ 1 1)
  (digit? \4)
  (println example-input)
  (grab-first-number-in-character-seq "aaa234yy55")  
  ,,,)

(defn -main
  "Main function"
  []
  ())
