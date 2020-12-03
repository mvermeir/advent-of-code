(ns aoc-2020.day-01a
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-01a.txt"
                 io/resource
                 io/reader
                 line-seq
                 (map read-string)))

(defn includes-2020-complement-fn [full-set]
  (fn [x]
    (let [set-excl-self (disj full-set x)]
      (set-excl-self (- 2020 x)))))

;; does not work when 1010 is the solution though
(defn solve-a [xs]
  (let [set-of-relevant (->> xs
                             (filter #(<= % 2020))
                             (into #{}))]
    (->> set-of-relevant
         (filter (includes-2020-complement-fn set-of-relevant))
         (reduce * 1))))

;; TESTS
(defn test-example [] (= (solve-a (list 1721 979 366 299 675 1456)) 514579))
(defn test-set-with-1010 []
  (= (solve-a
       (list 1010 1721 979 366 299 675 1456))
    514579))
(defn test-set-with-1010-twice []
  (= (solve-a
       (list 1010 1721 979 366 300 1010 1456))
    1010))

(defn -main
  "Main function"
  [& args]
  (println (solve-a input)))