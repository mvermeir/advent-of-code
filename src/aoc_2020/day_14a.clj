(ns aoc-2020.day-14a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-14a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                     "mem[8] = 11"
                     "mem[7] = 101"
                     "mem[8] = 0"))

(defn or-mask-from [mask-str]
  (-> (s/replace mask-str #"X" "0")
      (Long/parseLong ,,, 2)))
(defn and-mask-from [mask-str]
  ;; Clojure bit operations all work with long as far as I can tell -> 64 bits, found no better way to prepend
  ;; the 28 1's that doesn't cause number format exception other than this
  (-> mask-str
      (s/replace ,,, #"1" "X")
      (s/replace ,,, #"0" "1")
      (s/replace ,,, #"X" "0")
      (Long/parseLong ,,, 2)
      (bit-xor ,,, -1)))

(defn parse-mask [mask-str]
  [:MASK (or-mask-from mask-str) (and-mask-from mask-str)])

(defn parse-mem [command]
  (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" command)]
    [:MEM address (Long/parseLong value)]))

(defn parse-command [line]
  (let [[v value] (s/split line #" = ")]
    (if (= v "mask")
      (parse-mask value)
      (parse-mem line))))

(defn apply-mask [[_ flip_on_mask flip-off-mask] target]
  (-> target
    (bit-or,,, flip_on_mask)
    (bit-and,,, flip-off-mask)))

(defn execute-command [[mask memory] [type _ _ :as cmd]]
  (condp = type
    :MASK
    [cmd memory]

    :MEM
    (let [[_ address value] cmd
          new-value (apply-mask mask value)]
      [mask (assoc memory address new-value)])))

;; reduce of the entire thing, keep the computer state in the accumulator value and update it
;; on that result add up all the values in the memory map
(defn solve [input]
  (->> input
       (map parse-command)
       (reduce execute-command [nil {}])
       peek
       vals
       (reduce + 0)))

;; TESTS
(defn test-solution [] (= (solve example-input) 165))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))