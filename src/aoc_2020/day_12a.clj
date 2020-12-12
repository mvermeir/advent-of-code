(ns aoc-2020.day-12a
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-12a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "F10"
                     "N3"
                     "F7"
                     "R90"
                     "F11"))

(defn add [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])
(defn scale [[x0 y0] s]
  [(* x0 s) (* y0 s)])

(def headings-by-name {"N" [0 1]
                       "S" [0 -1]
                       "E" [1 0]
                       "W" [-1 0]})

(defn move [position heading clicks]
  (add position (scale heading clicks)))

(defn turn [heading degrees orientation]
  (let [nb-quarter-turns (/ degrees 90)]
    (->> orientation
         (drop-while #(not= heading %))
         (drop nb-quarter-turns)
         (take 1)
         first)))
(defn turn-left-by [heading degrees]
  (turn heading degrees (cycle '([1 0] [0 1] [-1 0] [0 -1]))))
(defn turn-right-by [heading degrees]
  (turn heading degrees (cycle '([1 0] [0 -1] [-1 0] [0 1]))))

(defn parse-navigation-instruction [line]
  (let [[_ direction amount-str] (re-matches #"(N|S|E|W|L|R|F)(\d+)" line)
        amount (Integer/parseInt amount-str)]
    (case direction
      ("N" "S" "E" "W")
      (fn [[position heading]]
        (let [new-position (move position (headings-by-name direction) amount)]
          [new-position heading]))

      "L"
      (fn [[position heading]]
        (let [new-heading (turn-left-by heading amount)]
          [position new-heading]))

      "R"
      (fn [[position heading]]
        (let [new-heading (turn-right-by heading amount)]
          [position new-heading]))

      "F"
      (fn [[position heading]]
        (let [new-position (move position heading amount)]
          [new-position heading])))))

(defn follow-course [nav-instr]
  (reduce
    (fn [state instr] (instr state))
    [[0 0] [1 0]]
    nav-instr))

(defn abs [n] (max n (- n)))

(defn solve [input]
  (->> (map parse-navigation-instruction input)
       follow-course
       first
       (map abs)
       (reduce +)))

;; TESTS
(defn test-solution [] (= (solve example-input) 25))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))