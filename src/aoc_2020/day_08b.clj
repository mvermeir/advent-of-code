(ns aoc-2020.day-08b
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "input-day-08a.txt"
                io/resource
                io/reader
                line-seq))

(def example-input (list
                     "nop +0"
                     "acc +1"
                     "jmp +4"
                     "acc +3"
                     "jmp -3"
                     "acc -99"
                     "acc +1"
                     "jmp -4"
                     "acc +6"))

(defn parse-instruction [line]
  (let [[_ op param] (re-matches #"(\w\w\w) ((?:\+|\-)\d+)" line)]
    [(keyword op) (read-string param)]))

(defn instr-pointer [state] (state :instruction-pointer))
(defn inc-instr-pointer [state x]
  (let [current (state :instruction-pointer)]
    (assoc state :instruction-pointer (+ current x))))
(defn inc-accumulator [state x]
  (let [current (state :accumulator)]
    (assoc state :accumulator (+ current x))))

(defn create-program [code-lines]
  (->> (map parse-instruction code-lines) vec))

(defn create-boot-loader [program]
  {:state      {:accumulator         0
                :instruction-pointer 0
                :mode                :unfinished}
   :history    #{}
   :operations {:nop  (fn [state _] (inc-instr-pointer state 1))
                :jmp  inc-instr-pointer
                :acc  (fn [state p] (-> state (inc-accumulator p) (inc-instr-pointer 1)))
                :err  (fn [state _] (assoc state :mode :loop-detected))
                :done (fn [state _] (assoc state :mode :finished))}
   :program    program})

(defn read-new-instruction [boot-loader]
  (let [program (boot-loader :program)
        state (boot-loader :state)
        history (boot-loader :history)
        instr (instr-pointer state)]
    (if (history instr)
      (vector :err 0)
      (if-let [instruction (get program (instr-pointer state))]
        instruction
        (vector :done 0)))))

(defn get-operation [boot-loader operation-type]
  ((boot-loader :operations) operation-type))

(defn execute [op state param] (op state param))

(defn record-executed [new-state {history :history state :state :as boot-loader}]
  (assoc boot-loader
    :history (conj history (instr-pointer state))
    :state new-state))

(defn run [{state :state :as boot-loader}]
  (let [[op-type param] (read-new-instruction boot-loader)
        {new-state :state :as new-boot-loader} (-> (get-operation boot-loader op-type)
                                                            (execute state param)
                                                            (record-executed boot-loader))]
    (if (#{:loop-detected :finished} (new-state :mode))
      new-state
      (recur new-boot-loader))))

(defn get-flippable-instruction-indexes [program]
  (->> (map-indexed (fn [idx item] [idx item]) program)
       (filter (fn [[_ [op _]]] (#{:jmp :nop} op)))
       (map first)))

(defn flip-instruction [idx program]
  (let [[op param] (get program idx)
        new-op (if (= :jmp op) :nop :jmp)]
    (assoc program idx [new-op param])))

(defn solve [input]
  (let [original-program (create-program input)
        flippable (get-flippable-instruction-indexes original-program)]
    (->> flippable
         (map #(flip-instruction % original-program))
         (map #(run (create-boot-loader %)))
         (filter (fn [{mode :mode :as state}] (= mode :finished)))
         (map (fn [state] (state :accumulator)))
         first)))

;; TESTS
(defn test-solution [] (= (solve example-input) 8))

(defn -main
  "Main function"
  [& args]
  (println (solve input)))