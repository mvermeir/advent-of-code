(ns aoc-2020.day-03b
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-03a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "..##......."
                     "#...#...#.."
                     ".#....#..#."
                     "..#.#...#.#"
                     ".#...##..#."
                     "..#.##....."
                     ".#.#.#....#"
                     ".#........#"
                     "#.##...#..."
                     "#...##....#"
                     ".#..#...#.#"))
(def example-slope [1 3])

(defn tree? [x] (= \# x))
(defn parse-terrain-type [x] (if (tree? x) :tree :clear))
(defn parse-map-line [line]
  (->> line
       (map parse-terrain-type)
       vec))
(defn parse-map [m]
  (->> m
       (map parse-map-line)
       vec))

(defn check-terrain-at [map x y]
  (when-let [row (get map x)]
    (get row y)))

(defn terrain-checker-fn [map infinite-to-right?]
  (let [y-fn (if infinite-to-right? #(mod % (count (get map 0))) identity)]
    (fn [[x y]]
      (check-terrain-at map x (y-fn y)))))

(defn walk-path
  ([slope map]
   (walk-path (terrain-checker-fn map true) slope (list [0 0])))
  ([checker slope path]
   (let [current-position (first path)
         current-terrain (checker current-position)
         next-position (vec (map + current-position slope))]
     (if current-terrain
       (recur checker slope (cons next-position (cons [current-position current-terrain] (rest path))))
       (rest path)))))

(defn nb-trees-for-slope [slope m]
  (->> (walk-path slope m)
       (map second)
       (filter #(= :tree %))
       count))

(defn solve [input slopes]
  (let [m (parse-map input)]
    (->> slopes
         (map #(nb-trees-for-slope % m))
         (reduce *))))

;; TESTS
(defn test-solution [] (= (nb-trees-for-slope example-slope (parse-map example-input)) 7))

(defn -main
  "Main function"
  [& args]
  (println (solve input (list [1 1] [1 3] [1 5] [1 7] [2 1]))))