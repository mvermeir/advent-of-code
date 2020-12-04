(ns aoc-2020.day-02a
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-02a.txt"
                 io/resource
                 io/reader
                 line-seq))

(defn create-validation-predicate [min-str max-str char-str]
  (let [min (Integer/parseInt min-str)
        max (Integer/parseInt max-str)
        char (get (char-array char-str) 0)]

    (fn [pwd]
     (let [char-frequencies (frequencies (seq pwd))
           policy-char-frequency (or (get char-frequencies char) 0)]
       (<= min policy-char-frequency max)))))

(defn parse-line [line]
  (let [[_ min-str max-str char-str pwd] (re-matches #"(\d+)-(\d+) (.+): (.+)" line)]
    [(create-validation-predicate min-str max-str char-str) pwd]))

(defn check-policy [[pred pwd]] (pred pwd))

(defn solve [input]
  (->> input
       (map parse-line)
       (filter check-policy)
       count))

;; TESTS
(def example-ok "1-3 a: abcde")
(def example-too-few "2-3 a: abcde")
(def example-too-many "1-3 a: abacadaea")
(def example-none "1-3 f: abcde")

(defn test-solution [] (= (solve (list example-ok example-too-few example-too-many example-none)) 1))
(defn test-with-example-from-site [] (= (solve (list "1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc")) 2))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))