(ns aoc-2020.day-04a
  (:require [clojure.java.io :as io]))

(def input (->> "input-day-04a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                     "byr:1937 iyr:2017 cid:147 hgt:183cm"
                     ""
                     "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                     "hcl:#cfa07d byr:1929"
                     ""
                     "hcl:#ae17e1 iyr:2013"
                     "eyr:2024"
                     "ecl:brn pid:760753108 byr:1931"
                     "hgt:179cm"
                     ""
                     "hcl:#cfa07d eyr:2025 pid:166559648"
                     "iyr:2011 ecl:brn hgt:59in"))

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

(def required-fields '("ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"))
(def optional-field "cid")
(defn valid? [passport]
  (every? passport required-fields))

(defn solve [input]
  (->> (parse-passports input)
       (filter valid?)
       count))

;; TESTS
(defn test-solution [] (= (solve example-input) 2))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))