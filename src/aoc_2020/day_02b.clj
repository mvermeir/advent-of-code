(ns aoc-2020.day-02b
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-02a.txt"
                 io/resource
                 io/reader
                 line-seq))

(defn create-validation-predicate [first-position-str second-position-str char-str]
  (let [first-position (dec (Integer/parseInt first-position-str))
        second-position (dec (Integer/parseInt second-position-str))
        char (get (char-array char-str) 0)]

    (fn [pwd]
     (let [first-char (get pwd first-position)
           second-char (get pwd second-position)]
       (cond
         (= first-char char) (not= second-char char)
         (= second-char char) (not= first-char char)
         :else false)))))

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
(defn test-with-example-from-site [] (= (solve (list "1-3 a: abcde" "1-3 b: cdefg" "2-9 c: ccccccccc")) 1))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))