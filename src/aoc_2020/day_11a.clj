(ns aoc-2020.day-11a
  (:require [clojure.java.io :as io]
            ))

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

(defn surrounding-spot-coordinates [[x y]]
  (list [(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
    [x (dec y)],,,,,,,,,,, [x (inc y)]
    [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]))

(defn occupied? [type] (= type :OCCUPIED))

(defn no-adjacent-are-occupied? [coord layout]
  (->> (surrounding-spot-coordinates coord)
       (map #(get layout %))
       (not-any? occupied?)))

(defn enough-adjacent-are-occupied? [coord layout]
  (->> (surrounding-spot-coordinates coord)
       (map #(get layout %))
       (filter occupied?)
       count
       (< 3)))

(defn apply-rules [[coord type] layout]
  (case type
    :FREE     (if (no-adjacent-are-occupied? coord layout)     :OCCUPIED :FREE)
    :OCCUPIED (if (enough-adjacent-are-occupied? coord layout) :FREE :OCCUPIED)
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
(defn test-solution [] (= (solve example-input) 37))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))