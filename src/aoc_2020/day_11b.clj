(ns aoc-2020.day-11b
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-11a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "L.LL.LL.LL"
                     "LLLLLLL.LL"
                     "L.L.L..L.."
                     "LLLL.LL.LL"
                     "L.LL.LL.LL"
                     "L.LLLLL.LL"
                     "..L.L....."
                     "LLLLLLLLLL"
                     "L.LLLLLL.L"
                     "L.LLLLL.LL"))

(defn parse-spot [spot]
  (case spot
    \L :FREE
    \. :NONE))
(defn parse-line [row-idx line]
  (map-indexed (fn [col-idx x] [[row-idx col-idx] (parse-spot x)]) line))
(defn parse-seat-layout [input]
  (->> (map-indexed parse-line input)
       (mapcat identity)
       (into (hash-map))))

(defn line-of-sight [[x0 y0] [x1 y1 :as direction]]
  (let [next [(+ x0 x1) (+ y0 y1)]]
    (lazy-seq (cons next (line-of-sight next direction)))))

(def direction-unit-vectors '([-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0]))

(defn occupied? [type] (= type :OCCUPIED))
(defn nothing? [type] (= type :NONE))

(defn look [los layout]
  (when-let [next-in-line-of-sight (layout (first los))]
    (if (nothing? next-in-line-of-sight)
      (recur (rest los) layout)
      next-in-line-of-sight)))

(defn no-occupied-in-sight? [coord layout]
  (->> direction-unit-vectors
       (map #(line-of-sight coord %))
       (map #(look % layout))
       (not-any? occupied?)))

(defn enough-occupied-in-sight? [coord layout]
  (->> direction-unit-vectors
       (map #(line-of-sight coord %))
       (map #(look % layout))
       (filter occupied?)
       count
       (< 4)))

(defn apply-rules [[coord type] layout]
  (case type
    :FREE     (if (no-occupied-in-sight? coord layout)     :OCCUPIED :FREE)
    :OCCUPIED (if (enough-occupied-in-sight? coord layout) :FREE :OCCUPIED)
    type)
  )

(defn simulate-new-round [layout]
  (->> layout
       (reduce
         (fn [changed [coord type :as spot]]
           (let [new-type (apply-rules spot layout)]
             (if (not= new-type type)
               (conj changed [coord new-type])
               changed)))
         [])
       (into layout)))

(defn simulate [layout]
  (let [new-layout (simulate-new-round layout)]
    (if (not= new-layout layout)
      (recur new-layout)
      new-layout)))

(defn solve [input]
  (->> (parse-seat-layout input)
       simulate
       vals
       (filter occupied?)
       count))

;; TESTS
(defn test-solution [] (= (solve example-input) 26))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))