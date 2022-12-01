(ns aoc-2021.day17
  (:require [util :as util]))

(defn apply-drag [horizontal-velocity]
  (cond
    (< horizontal-velocity 0) (inc horizontal-velocity)
    (> horizontal-velocity 0) (dec horizontal-velocity)
    :else 0))
(defn apply-gravity [vertical-velocity] (dec vertical-velocity))

(defn simulate-trajectory [[x y :as position] [vx vy]]
  (let [next-position [(+ x vx) (+ y vy)]
        next-velocity [(apply-drag vx) (apply-gravity vy)]]
    (cons position (lazy-seq (simulate-trajectory next-position next-velocity)))))

(defn target [min-x max-x min-y max-y]
  (->> (for [x (range min-x (inc max-x))
             y (range min-y (inc max-y))]
         [x y])
       (into #{})))

(defn parse-target [line]
  (let [[_ min-x max-x min-y max-y] (re-matches #"target area: x=([-]?\d+)..([-]?\d+), y=([-]?\d+)\.\.([-]?\d+)" line)]
    min-y))

(comment "repls tests"
  [[vx, vy] [x y]]

  (drop 7 (take 8 (simulate-trajectory [0 0] [7 2])))
  (boolean ((target 20 30 -10 -5) [21 -11]))

  ;; TODO: make trajectories stop, then take the highest point 
  ;; TODO: if multiple x hit the target does it matter which? I guess not since only y affects height
  (drop 7 (take 8 (simulate-trajectory [0 0] [7 2])))
  "strategy: adjust so you find an x that hits the target. since parabola -> highest one will always"

  (def bits-example-1 (mapcat hex->bits example-1))

  (re-matches #"target area: x=([-]?\d+)..([-]?\d+), y=([-]?\d+)\.\.([-]?\d+)" "target area: x=281..311, y=-74..-54")
  (parse-target "target area: x=281..311, y=-74..-54")

  ,)


(defn -main
  "Main function"
  []
  (println (let [min-y (->> (util/file->str "2021/d17.txt")
                            parse-target
                            util/abs)]
             (/ (* min-y (dec min-y)) 2))))