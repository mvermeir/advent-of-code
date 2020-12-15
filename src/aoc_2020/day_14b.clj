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

(defn or-mask-from [mask-str]
  (-> (s/replace mask-str #"X" "0")
    (Long/parseLong ,,, 2)))

(defn find-floater-indexes [mask-str]
  (let [max-bit-index (dec (count mask-str))
        bit-indexed-chars (map-indexed (fn [index x] [(- max-bit-index index) x]) mask-str)]
    (reduce
     (fn [float-indexes [bit-index x]]
       (if (= x \X)
         (conj float-indexes bit-index)
         float-indexes))
     []
     bit-indexed-chars)))
(defn parse-mask [mask-str]
  [:MASK (or-mask-from mask-str) (find-floater-indexes mask-str)])

(defn parse-mem [command]
  (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" command)]
    [:MEM (Long/parseLong address) (Long/parseLong value)]))

(defn parse-command [line]
  (let [[v value] (s/split line #" = ")]
    (if (= v "mask")
      (parse-mask value)
      (parse-mem line))))

(defn apply-mask [[_ base-mask floater-indexes] target]
  (let [base-address (bit-or base-mask target)]
    (reduce
      (fn [addresses-so-far floater-index]
        (concat
          (map #(bit-set % floater-index) addresses-so-far)
          (map #(bit-clear % floater-index) addresses-so-far)))
      [base-address]
      floater-indexes)))

(defn update-memory [memory addresses value]
  (->> (map #(vector % value) addresses)
       (into memory)))

(defn execute-command [[mask memory] [type _ _ :as cmd]]
  (condp = type
    :MASK
    [cmd memory]

    :MEM
    (let [[_ address value] cmd
          addresses-to-update (apply-mask mask address)]
      [mask (update-memory memory addresses-to-update value)])))

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