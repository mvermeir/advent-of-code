(ns aoc-2020.day-14b
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-14a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "mask = 000000000000000000000000000000X1001X"
                     "mem[42] = 100"
                     "mask = 00000000000000000000000000000000X0XX"
                     "mem[26] = 1"))

(defn generate-all-masks [mask-str]
  (let [max-bit-index (dec (count mask-str))
        bit-indexed-chars (map-indexed (fn [index x] [(- max-bit-index index) x]) mask-str)]
    (reduce
     (fn [masks-so-far [bit-index x]]
       (condp = x
         \1 (map #(bit-set % bit-index) masks-so-far)
         \X (concat masks-so-far (map #(bit-set % bit-index) masks-so-far))
         masks-so-far))
     [0]
     bit-indexed-chars)))
(defn parse-mask [mask-str]
  [:MASK (generate-all-masks mask-str)])

(defn parse-mem [command]
  (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" command)]
    [:MEM address (Long/parseLong value)]))

(defn parse-command [line]
  (let [[v value] (s/split line #" = ")]
    (if (= v "mask")
      (parse-mask value)
      (parse-mem line))))

(defn apply-mask [[_ masks] target]
  (println masks)
  (map #(bit-or target %) masks))

(defn execute-command [[mask memory] [type _ _ :as cmd]]
  (condp = type
    :MASK
    [cmd memory]

    :MEM
    (let [[_ address value] cmd
          new-value (apply-mask mask value)]
      [mask (assoc memory address new-value)])))

(defn solve [input]
  (->> input
       (map parse-command)
       (reduce execute-command [nil {}])
       peek
       vals
       (reduce + 0)))

;; TESTS
(defn test-solution [] (= (solve example-input) 208))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))