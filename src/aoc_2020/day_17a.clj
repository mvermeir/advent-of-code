(ns aoc-2020.day-17a
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (->> "input-day-17a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     ".#."
                     "..#"
                     "###"))

(defn parse-line [x line]
  (map-indexed
    (fn [y spot-value]
      (if (= spot-value \#) [x y 0] nil))
    line))

(defn parse-initial-layer [input]
  (->> (map-indexed parse-line input)
       (mapcat identity)
       (filter seq)
       (into (hash-set))))

(defn surrounding-coordinates [[x0 y0 z0]]
  (for [x [(dec x0) x0 (inc x0)]
        y [(dec y0) y0 (inc y0)]
        z [(dec z0) z0 (inc z0)]
        :when (or (not= x x0) (not= y y0) (not= z z0))]
    [x y z]))

(defn remains-active [active-cubes coord]
  (if (active-cubes coord)
    (->> (surrounding-coordinates coord)
         (filter #(active-cubes %))
         count
         ((fn [nb-active] (or (= nb-active 2) (= nb-active 3)))))))

(defn becomes-active [active-cubes coord]
  (if (not (active-cubes coord))
    (->> (surrounding-coordinates coord)
         (filter #(active-cubes %))
         count
         (= 3))))

(defn simulate-cycle [active-cubes]
  (let [relevant-coordinates (->> active-cubes
                                  (mapcat surrounding-coordinates)
                                  (reduce conj #{}))]
    (->> relevant-coordinates
         (filter #(or (remains-active active-cubes %) (becomes-active active-cubes %)))
         (reduce conj #{}))))

(defn simulate
  ([initial-state]
   (simulate initial-state 6))
  ([state remaining-cycles]
   (if (not= remaining-cycles 0)
     (recur (simulate-cycle state) (dec remaining-cycles))
     state)))

(defn solve [input]
  (->> (parse-initial-layer input)
       simulate
       count))

;; TESTS
(defn test-solution [] (= (solve example-input) 112))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))