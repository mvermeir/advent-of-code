(ns util
  (:require [clojure.java.io :as io]))

(defn to-int [str]
  (Integer/parseInt (re-find #"\A-?\d+" str)))

(defn char->int [ch]
  (if (and (char? ch) (java.lang.Character/isDigit ch))
    (java.lang.Character/getNumericValue ch)
    nil))

(defn str->long [str]
  (Long/parseLong (re-find #"\A-?\d+" str)))

(defn file->seq [file-name]
  (->> file-name
       io/resource
       io/reader
       line-seq))

(defn file->str [file-name]
  (->> file-name
       io/resource
       io/reader
       slurp))

(defn split-using [predicate col]
  (filter (fn [xs] (not (predicate (first xs)))) (partition-by predicate col)))

(defn abs [^long x]
  (Math/abs x))

(defn normalize [xs]
  (->> (map (fn [x]
              (if (= x 0) 0 (/ x (abs x))))
         xs)
       (into [])))

(defn parse-grid-map [value-fn grid-lines]
  (->> (map-indexed
         (fn [row-idx row]
           (map-indexed
             (fn [col-idx x] [[row-idx col-idx] (value-fn x)]) row))
         grid-lines)
       (apply concat)
       (into {})))

(defn generate-surrounding-coordinates [[row col :as center]]
  (for [x [(dec row) row (inc row)]
        y [(dec col) col (inc col)]
        :when (not= center [x y])]
    [x y]))

(defn upper-case? [^String string]
  (every? (fn [char] (Character/isUpperCase ^char char)) string))
