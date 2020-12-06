(ns aoc-2020.day-04b
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-04a.txt"
                io/resource
                io/reader
                line-seq))

(defn end-off-passport? [line] (re-matches #"\s*" line))

(defn parse-fields [line]
  (->> (re-seq #"(\S+):(\S+)?" line)
       (map #(subvec % 1))))

(defn parse-passports
  ([raw-lines]
   (parse-passports '() raw-lines))
  ([passports remaining-lines]
   (if (seq remaining-lines)
     (let [line (first remaining-lines)
           current-passport (if (seq passports) (first passports) {})]
       (if (end-off-passport? line)
         (if (seq current-passport)
           (recur (cons {} passports) (rest remaining-lines))
           (recur passports (rest remaining-lines)))
         (recur (cons (into current-passport (parse-fields line)) (rest passports)) (rest remaining-lines))))
     passports)))

(defn validate-x-digit-nb-between [x digits min max]
  (let [regex (re-pattern (apply str (repeat digits "\\d")))]
    (and (re-matches regex x)
     (<= min (Integer/parseInt x) max))))
(defn validate-birth-year [x]
  (validate-x-digit-nb-between x 4 1920 2002))
(defn validate-issuer-year [x]
  (validate-x-digit-nb-between x 4 2010 2020))
(defn validate-expiration-year [x]
  (validate-x-digit-nb-between x 4 2020 2030))
(defn validate-height [x]
  (when-let [[_ nb unit] (re-matches #"(\d+)(in|cm)" x)]
    (if (= unit "in")
      (<= 59 (Integer/parseInt nb) 76)
      (<= 150 (Integer/parseInt nb) 193))))
(defn validate-hair-color [x]
  (re-matches #"#[0-9a-f]{6}" x))
(defn validate-passport-id [x]
  (re-matches #"\d{9}" x))
(defn validate-eye-color [x]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} x))

(defn create-validator [key validator-fn]
  (fn [passport] (when-let [value (passport key)] (validator-fn value))))

(def validators (list
                  (create-validator "byr" validate-birth-year)
                  (create-validator "iyr" validate-issuer-year)
                  (create-validator "eyr" validate-expiration-year)
                  (create-validator "hgt" validate-height)
                  (create-validator "hcl" validate-hair-color)
                  (create-validator "ecl" validate-eye-color)
                  (create-validator "pid" validate-passport-id)))

(defn valid? [passport]
  (every? (fn [validator-fn] (validator-fn passport)) validators))

(defn solve [input]
  (->> (parse-passports input)
       (filter valid?)
       count))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))